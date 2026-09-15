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

---

# Follow-up: what was changed, and what was not

**Date:** 2026-09-15 · **Branch:** `claude/workspace-sessions-ux-fixes`
(branched from the commit that added this report).

The 18 findings were not treated as 18 defects. Reproducing each one against
`target/debug/fresh` first — the same tmux-driven method the test used — showed
that several share a single cause, and that four of them describe behaviour that
works. What follows is grouped by cause, with the design decision recorded for
each group, then the declines.

## Scoreboard

| Finding | Sev | Outcome |
| --- | --- | --- |
| F1 host-key trust-on-first-use | 4 | **Fixed** — verified interactively |
| F2 partial `~/.ssh/config` | 4 | **Fixed** — verified interactively + regression test |
| F3 truncated error text | 3 | **Fixed** — verified interactively |
| F4 recovery hidden behind right-click | 3 | **Fixed** — verified interactively |
| F5 checkbox/caption/behaviour disagree | 3 | **Fixed** — verified interactively |
| F6 "leave empty to use provided branch" | 3 | **Fixed** — verified interactively |
| F7 auto-name collides | 2 | **Fixed** — verified interactively |
| F8 undisclosed writes to the repo | 2 | **Fixed** — verified interactively |
| F9 delete keeps the branch, undocumented | 2 | **Fixed** — verified interactively |
| (follow-up) Delete had no worktree choice | — | **Fixed** — verified interactively |
| F10 no warning on quit | 2 | **Declined for now** — out of this feature |
| F11 Ctrl+P does nothing | 2 | **Not a defect** — it works (see below) |
| F12 no keyboard route to the dock | 2 | **Mostly not a defect** — `Alt+O` is bound |
| F13 `▼` inert, Esc dead, `←→` wrong | 2 | **One third fixed**, two thirds not defects |
| F14 modal is mostly empty space | 2 | **Not fixed** — reservation made *more* honest |
| F15 labels inconsistent and over-long | 1 | **Not fixed** |
| F16 status glyphs have no legend | 1 | **Not fixed** |
| F17 worktree paths unpreviewed | 1 | **Fixed** — verified interactively |
| F18 assorted polish | 1 | **Not fixed** |

## Group A — reaching another machine (F1 + F2)

**One cause, two symptoms.** `formSshArgv` resolved a `~/.ssh/config` alias
itself into `user@hostname:port` and handed ssh *that*. A resolved destination
matches no `Host` block, so every directive beyond the three the plugin's own
parser reads (`HostName`, `User`, `Port`) silently stopped applying —
`IdentityFile`, `IdentitiesOnly`, `UserKnownHostsFile`, `StrictHostKeyChecking`,
`ProxyJump`, `ProxyCommand`. That is F2 exactly, including the tester's
`Permission denied (publickey)` on the very next attempt. And because no
`StrictHostKeyChecking` was set while `BatchMode=yes` was, an unknown host key
came back as a bare `Host key verification failed.` — F1.

Worth recording: `formSshArgv` was the *only* caller in the file doing this.
`machineForHost` and `captureCreateSpec` already passed the alias, and
`machineForHost` even carries a comment saying why ("ssh resolves user, port and
identity from the entry, so the alias is the whole target"). This was an
outlier, not a design.

**F2 — the decision.** The report offered two options: honour more directives
ourselves, or delegate to the system `ssh`. Delegating won, and not narrowly:

- We already shell out to `ssh` for every connection, so a second interpreter of
  the same file is a second source of truth that can disagree with the one
  actually connecting. The tester's whole complaint is that the two halves
  disagreed.
- The list in the recommendation (`IdentityFile`, `IdentitiesOnly`,
  `UserKnownHostsFile`, `StrictHostKeyChecking`, `ProxyJump`) is not closed.
  `Match`, `Include`, `CanonicalizeHostname`, `ProxyCommand`, `%h`/`%p` tokens
  and the system-wide `/etc/ssh/ssh_config` are all part of the same grammar.
  Any line we stop at recreates the "which half is live?" problem at a new
  boundary.
- Delegating is a *smaller* change than parsing more: the plugin's parser
  shrinks in responsibility to what it is good at — listing aliases for the
  picker and rendering the `↳ root@127.0.0.1:2222` hint — and stops deciding
  how to connect.

Where the plugin genuinely needs resolved facts (the address to scan for a
fingerprint, which `known_hosts` an accept will write to), it asks `ssh -G`
rather than re-reading the file.

**F1 — the decision.** Two designs were weighed. The cheap one is to add
`StrictHostKeyChecking=accept-new` to the plugin's ssh calls, matching what the
Rust agent carrier already does; it is one line and the task succeeds with zero
clicks. It was rejected because it silently trusts, and the thing being trusted
is the identity of a machine the user is about to run commands on. The report is
right that a fingerprint confirmation is what every other ssh client does.

So the create catches that one error — narrowly, per the repo's own rule about
recovery paths — and asks:

```
┌ Unknown host key ──────────────────────────────────────────────┐
│ This computer has never connected to testbox before.           │
│                                                                │
│   Connects to  127.0.0.1 port 2222                             │
│   Host key     256 SHA256:F2Rk9zYU2ip4…i1SeoE (ED25519)        │
│                                                                │
│ Trusting records the key in /root/sshtest/etc/known_hosts, and │
│ this host connects without asking again.                       │
│ Continue only if this fingerprint is the one you expect.       │
│                                                                │
│ [ Cancel ] Esc    [ Trust and connect ]                        │
└────────────────────────────────────────────────────────────────┘
```

Cancel sits first and holds the keyboard, so Enter never trusts by reflex —
copying the delete dialog the report singled out as exemplary.

Accepting re-runs ssh with `StrictHostKeyChecking=accept-new` rather than
appending to `known_hosts` here. Which file the line belongs in is the user's
config to decide, and it may be hashed; reimplementing that lookup would be a
second answer to a question ssh already answers. The dialog above is proof it
works: `/root/sshtest/etc/known_hosts` is the *non-default* path from the
fixture's config, and that is where the key landed.

**Deliberately not changed:** the Rust agent carrier keeps its blind
`accept-new`. It is also reached by `fresh ssh://host/path` from the command
line, where there is no dialog and no one to ask, and narrowing it there would
turn working invocations into failures for a problem this report did not raise.
In the orchestrator flow it is now moot — the gate runs first, so the host is
either trusted or explicitly refused before the carrier connects. Worth revisiting
separately.

**Where the probe was lying too.** The form's own note rendered a host-key
failure as "Host unreachable — the worktree is still attempted on create", which
sends the user to check the network. It now says the key is not trusted yet and
that Create will ask. Likewise `machine.hint_hostkey` no longer tells the user to
"connect once from a shell" — that was the out-of-band workaround this removes.

**Acceptance bar, met.** With the report's fixture — a host never connected to,
a non-default `IdentityFile`, a non-default `UserKnownHostsFile`, no pre-seeded
`known_hosts`, no key copied to `~/.ssh/id_ed25519` — a workspace session is
created after one confirmation click, without leaving the application. Checked
afterwards: the key is in the config's `known_hosts`, `~/.ssh/` still contains
only `config`, and the remote worktree exists.

## Group B — a failed row was a dead end (F3 + F4)

Both findings are the same mistake: at the one moment a user needs a way out,
the product offered neither the information nor the action. The reason was cut
to the dock width (`Host key v…`) and the only remedy lived in an
undiscoverable right-click menu.

Fixing them separately would have meant a tooltip *and* a `⋯` button. Instead
the row keeps its one-line summary and a panel under the tree carries what does
not fit — the full reason, wrapped, never elided — with the two actions as
buttons:

```
────────────────────────────────
 host key not trusted — nothing was
 connected to
 [ Retry ] [ Dismiss ]
```

`failPending` moves the dock's highlight onto the row it just failed, so the
panel is on screen *when the failure happens* rather than after the user has
gone looking for it. The highlight is not the active window, so nothing the user
is working in moves. The panel occupies rows only while a failed or paused row is
selected — a dock with nothing wrong is exactly as tall as before.

## Group C — controls that misdescribed themselves (F5 + F6 + F17)

**F5 is a wording bug, not a logic bug.** The glyph said `[v]`, the caption said
"Not checked", and the behaviour created a worktree. Two of those three agree:
the caption's "checked" meant the *repository probe*, not the control. So the
fix is one word, not a redesign — the note now says "Repository not verified".
No behaviour changed, because none was wrong.

**F6 is a real lie**, and the interesting question was how to stop telling it.
With both branch fields blank the create runs
`git worktree add … -b <workspace name> <default branch>`, so "leave empty to
use provided branch" is the opposite of the truth. Restating the rule accurately
was rejected: it has three arms, two of which depend on fields above it, and a
sentence about behaviour drifts from behaviour. The form names the outcome
instead, live as the fields are typed:

```
New branch name: [                    ]
                 ↳ blank: new branch acme-widgets-1, cut from HEAD
                 ↳ made at /root/.local/share/fresh/orchestrator/…/acme-widgets-1
```

Type `fix/login` and the first line becomes `new branch fix/login, cut from
HEAD`; type `main` into Checkout branch and it becomes `cut from main`. Verified
against reality afterwards: `git branch` showed `fix/login` and `git worktree
list` showed the previewed directory.

The second line is F17 for free — the same moment, the same question ("what is
about to happen to my repository?"), so it would have been artificial to answer
one and not the other.

## The rest

**F8 — fixed, and the report was right.** The worktree and branch belong to
cross-machine session recovery: every lifecycle action that mutates the archive
manifest records the session list on `refs/heads/<user>/fresh-sessions`, which is
maintained through a worktree of its own at
`<data dir>/orchestrator/.sync-workspace` so it does not disturb the user's
`git worktree` set. It fires on archive, delete and unarchive — not on create,
which is why it does not show up if you only look after making a workspace.

The report asked for a one-line disclosure. It goes in the archive and delete
confirmations, for the same reason F9's line does: those dialogs already
enumerate what the action does, and a list that careful reads as "and nothing
else". Writing a branch into the user's repository is a
larger omission than the surviving branch was. (At the time it was also pushed
to origin; that part is gone now.) The branch name in the line is
derived, not hard-coded, so it names the ref the user will actually find:

```
This will:
  • stop all workspace processes
  • run `git worktree remove`
  • drop the workspace record
  • keep the branch — `git worktree remove` does not delete it
  • update fresh/fresh-sessions — Fresh's own session list, kept on this machine
```

The line said "pushed to origin" when this was first written, because that is
what happened. The push has since been removed — see "Investigated before
touching: what the `fresh/fresh-sessions` push does" below — so the line now
says where the list stays instead.

**F7 — fixed.** The counter advanced only inside `runLocalCreate`; a remote
create bakes its name in at capture and never goes through there, and the branch
scan that backs the counter up cannot see a worktree cut on another machine. A
remote create now claims its name at submit. Verified: local, then remote, then
reopening gives `-1`, `-2`, `-3` with no repeat and no skip.

**F9 — fixed with a line, not a checkbox.** The report suggested an "also delete
branch" checkbox. Keeping the branch is the right default — it may hold the only
copy of the work — and the dialog's problem was that it enumerated everything
*except* this, which reads as "and nothing else". So the enumeration got its
missing item: "keep the branch — `git worktree remove` does not delete it". A
checkbox would be a new control for a default that is already correct.

(The dialog *did* subsequently gain a checkbox, but for the worktree rather than
the branch — see below. The distinction is that removing the worktree is a
destructive act the user may reasonably not want, whereas deleting the branch is
not something Delete has ever offered to do.)

**Bonus fix found on the way.** The form reserves a constant height so nothing
moves when a section changes shape, but it measured that reservation from the
live form — including three values that arrive from async probes. The
reservation therefore grew when the probes landed and the dialog re-centred a
second after opening. It was invisible only because the rows in question
happened to be the same number either way; the branch preview made it visible and
an existing test caught it. The probe now pins every input the height depends on.

## Declined, with reasons

**F10 (quitting kills a live remote session with no warning) — declined for
now.** The finding is fair and the inconsistency with F9 is real. It is declined
on scope, not merit: the quit path is core editor lifecycle, shared by every
window and buffer, and a confirmation there is a decision about the editor's quit
semantics rather than about workspace sessions. Doing it properly means deciding
what counts as "work in progress" for every session kind, which is a larger
design than this report's remit. Worth its own issue.

**F14 (the modal is mostly empty space) — not fixed, and partly by design.** The
blank bands are deliberate: the form reserves each section at its tallest shape so
that changing Machine or the agent never moves the rows below. That reservation
is the reason the tester's other complaint in the same finding ("fields jump
position") is *not* what happens for most changes. Making the dialog tight would
trade a stable layout for a compact one, which is the wrong way round for a form
people fill in repeatedly. Two of the reserved rows are now filled with the
previews above, and the reservation itself was made honest (see the bonus fix), so
the symptom is smaller. A real fix — reserving less by making the sections
genuinely uniform — is a layout redesign, not a patch.

**F8 was first written up here as "could not reproduce". That was wrong**, and
the mistake is worth recording because of how it happened: the first pass checked
`git worktree list` after *creating* workspaces, and the sync fires on
**archive and delete**. A later manual pass deleted a workspace and
`.sync-workspace` with `fresh/fresh-sessions` appeared immediately, exactly as
the report describes, branch name and all. See below — it is fixed.

**F15, F16, F18 — not fixed.** Severity 1, and each is a genuine (if cosmetic)
improvement. F18's dropdown bleed-through and the box-in-box confirm border are
host-renderer issues rather than orchestrator ones; both are visible in the
screenshots above and should be filed against the widget layer.

## Found while re-testing, not in the original 18 — fixed

**Deleting a *remote* workspace left its worktree on the host.** The confirm
dialog says it will "run `git worktree remove`", and for a local workspace it
does. For an SSH workspace the row disappeared from the dock but the remote
worktree stayed registered and on disk, so the dialog promised something that
never happened and every delete leaked a directory onto the host.

The cause is the same `ownsWorktree` signal the rest of the lifecycle leans on:
it answers `projectPath !== root`, and the host records no separate project for
a remote session, so `removable` was false and nothing ran.

The fix turns on one property of the plugin API that makes it small:
`spawnProcess` routes through the **active** authority. Doing the removal while
the session's own window is still in front therefore runs `git` on the far side
with no ssh argv to rebuild — the plugin never sees a live session's transport
(`WindowInfo.remote` carries a display identity, not an identity file), and
reconstructing one would be guessing. It is the exact inverse of
`createRemoteWorktree`, which also ran on the far side. `git -C <worktree>
worktree remove <worktree>` lets the worktree remove itself through its own
common dir, so the repository root — which this side does not know — is never
needed either.

**What keeps it safe.** A remote session may equally be the user's *actual
project directory*, opened with the worktree toggle off; removing that would
destroy their work. Two independent facts must both hold before anything is
removed: the path is under Fresh's own `~/.fresh/worktrees/`, and git itself
says it is a *linked* worktree (`--git-dir` differs from `--git-common-dir`)
rather than a main checkout. Verified both ways — a workspace created with the
toggle on is removed from the host; one created with the toggle off, pointed at
a real project, is left completely intact (files, history and worktree
registration all present after the delete).

A host that cannot be reached does not block the delete: the row goes either
way, and the status bar names the path still sitting on the far side rather than
failing silently.

## The Delete confirmation's worktree checkbox

Delete always removed the worktree, and the dialog described that as a fact
rather than a choice. Two things were wrong with it beyond the missing choice:

- On an **in-place or shared-tree session** — the project row itself, say — the
  pane still announced "run `git worktree remove`" and "keep the branch", and
  warned that "uncommitted changes will be lost". None of it happens: that
  delete drops the workspace record and touches no files at all.
- There was no way to say "I'm done with this session but I want to keep the
  files", which for a worktree holding real work is a reasonable thing to want.

So the pane now asks, and only where the question means anything:

```
This will:                                    This will:
  • stop all workspace processes                • stop all workspace processes
  • run `git worktree remove`                   • leave the worktree and its files where they are
  • keep the branch — … does not delete it      • drop the workspace record
  • drop the workspace record                   • update fresh/fresh-sessions — …
  • update fresh/fresh-sessions — …
                                              [ ] Also remove the worktree and its files
Uncommitted changes will be lost.
                                              [ Cancel ]  [ Confirm Delete ]
[v] Also remove the worktree and its files

[ Cancel ]  [ Confirm Delete ]
```

Checked by default, because removing the worktree is what Delete has always
done: this adds a way to keep the files, it does not quietly change what the
button means. The consequence list is rebuilt from the checkbox, so the dialog
never describes an action other than the one the buttons will take — and the
"uncommitted changes will be lost" warning appears only when files are really
going, since a warning that cries wolf is worse than none.

The checkbox is absent entirely when there is no worktree to remove, which is
the same test the removal itself uses: `ownsWorktree` for a local session, and
the linked-worktree-under-`~/.fresh/worktrees/` pair for a remote one. Offering
a switch wired to nothing would imply the delete does something it does not.

The choice is passed into `deleteOne` as an argument rather than read from the
dialog's state, so the plugin API's own delete keeps the long-standing behaviour
and the checkbox cannot leak out of the dialog that asked for it.

Verified by hand across all four combinations: local worktree and remote
worktree, each with the box checked (worktree and its registration removed, the
branch surviving) and unchecked (worktree, files and registration all intact,
only the dock row gone); and a no-worktree session, which shows no checkbox and
none of the worktree lines.

## Code review, and what it caught

A review pass over the finished branch found six problems. Five were real and
are fixed; all are worth recording, because three of them are mistakes the
manual testing above walked straight past.

**The branch preview still lied in one of its three arms.** The create has
three: a typed new-branch name cuts it off the base; a typed *checkout* branch
with no new-branch name checks that branch out and cuts nothing; both blank cuts
`<workspace>` off the default. The preview keyed off "is there a base", so the
middle arm — the common "put me on `feature`" case — was described as "blank:
new branch acme-widgets-1, cut from feature". No branch is created there at all.

This one is uncomfortable, because the manual pass *saw* it: a session was
created on `feature` when the preview had named a new branch, and that was
written off as "the New branch name field must have been cleared". The evidence
was on screen and got explained away. The preview now takes the typed base and
the placeholder default as separate arguments, because which arm runs turns on
whether Checkout branch was *typed*, not on what the field displays — and the
third arm says "checks out feature — no new branch is created". Re-verified
against git: worktree on `feature`, no new branch.

**The remote worktree removal could have deleted local files.** It runs over the
session's own authority by making its window active first — but
`PluginCommand::SetActiveWindow` on a *disconnected* remote session installs an
empty local shell and starts the connect *afterwards*, so a spawn issued right
after the switch races it and is served by the **local** spawner instead. It
would then run `git worktree remove --force` against a remote absolute path on
this machine. Which is not hypothetical here: this test rig uses `127.0.0.1` as
the "remote", so the path exists locally too.

The fix refuses to act unless the host says the session is connected, and the
first attempt at that was itself wrong — it tested our own `remote.state`, which
a freshly created session leaves at `"starting"` for ever (nothing promotes it),
so it blocked exactly the live sessions it was meant to allow. It asks
`WindowInfo.remote.connected` now, which is the host's own answer. Verified both
ways: connected deletes remove the worktree; a delete after the connection was
killed leaves all of it untouched and says so.

**The trust dialog was only wired into the worktree path.** With the worktree
toggle off there is no remote `git` step, so the create went straight to
`attachRemoteAgent` — whose carrier trusts an unknown key on its own — while the
form had already promised "you'll be asked to confirm it on create". The
reviewer expected a hard failure; what actually happened was worse in kind if
not in effect: it succeeded, silently trusting a host the user never saw the
fingerprint of, which is the whole thing F1 was about. The gate now runs for
every ssh create, at the cost of one round trip in the no-worktree case.

**The auto-name counter could be driven by a typed name.** `claimAutoSessionName`
took whatever name the spec carried and advanced the global counter past any
trailing digits, so naming one workspace `release-2026` would have made every
later auto-name in every project `<project>-2027`. It is now called only when
the user left the field blank *and* the spec's name is the one the form
generated.

**The failure panel's height estimate could clip the dock.** It divided the
message width by the column count; the host word-wraps with a hanging indent, so
a real `Permission denied (publickey,…)` takes more rows than that, and the
tree over-allocated into the dock's last row — the exact failure the panel was
added to prevent. It now packs words the way the renderer does.

**Two doc comments were separated from their functions** by inserting new code
between a comment and the item it documented (`ownsWorktree` in the plugin,
`slow_fake_ssh_on_path` in the test helpers). Both restored.

Also fixed while there: the "left behind" message was written with `setStatus`
inside `deleteOne` and then immediately overwritten by the batch's own
"deleted 1 workspace(s)" summary, so the one case where the user most needed
telling said nothing. It travels out on the result now and replaces the summary.
And the confirm dialog no longer promises `git worktree remove` for a host it
cannot reach — it says the worktree stays on the host, and why.

## Findings that are not defects

The tester was a first-time user with no documentation, which is the point of the
method — but it also means a thing that looks broken may only be unfamiliar. Four
claims did not survive re-testing.

**F11 — "the one advertised global shortcut did not work". It works.** The
status bar reads `Palette: Ctrl+P`, and Ctrl+P opens the palette. It renders
**at the bottom of the screen**, under a `file | >command | :line | #buffer`
scope row — so a reader looking at the top of the terminal sees nothing happen.
Confirmed with the editor focused *and* with the dock focused, and confirmed that
tmux really delivers `0x10` (`cat -v` shows `^P`). No change made. If anything is
worth doing here it is making the palette's arrival more visible, which is a
different finding from the one filed.

The second half of F11 — that when a workspace terminal holds focus, nothing
distinguishes a keystroke going to the app from one going to the remote shell —
is a fair observation and is **not** addressed here.

**F12 — "no keyboard route to the Orchestrator dock". `Alt+O` is bound** in
`keymaps/default.json` (`toggle_dock_focus`, in five contexts), and it works:
pressed with the dock hidden it opens *and* focuses it. The dock's title even
underlines the `O` in "Orchestrator" as the mnemonic, derived from that binding.
The palette also carries `Orchestrator: Toggle Dock`, `Orchestrator: Open` and
`Orchestrator: New…`. The claim that "the View menu shows no shortcuts at all" is
also wrong — it shows `Ctrl+B` for File Explorer.

What *is* true, and is the whole of the real finding: the `Orchestrator Dock` row
sits directly beneath `File Explorer  Ctrl+B` and shows no accelerator of its own,
so a reader concludes there isn't one. The cause is that menu accelerators are
looked up by the row's action (`orchestrator_dock_toggle`, unbound) while the key
is on a different action (`toggle_dock_focus`). Fixing it properly means letting a
plugin declare a row's accelerator, which is a new field on the public
`AddMenuItemOptions` plus schema and `.d.ts` regeneration — a public API change for
a label. **Declined at that price**, and recorded here so the next person does not
have to rediscover the cause.

**F13 — two of its three parts are not defects.** Clicking the `▼` opens the
list (verified at the exact display column; note the focus marker `▸` appears on
the row when the control takes focus, which is a plausible source of a one-column
mis-click). `Esc` with the list open closes the list and leaves the form open.

The third part is real and is **fixed**: the footer promises `←→ change option`
without qualification, but the dropdown answered Left/Right only while its list
was closed. Left/Right now do the same thing open as closed, rather than the
footer growing a special case for a state it should not have to know about.

## Verification

Everything marked "verified interactively" was driven through the real binary in
tmux with SGR mouse events and `capture-pane`, against the report's own fixture
(user-space `sshd` on `127.0.0.1:2222`, ed25519, pubkey-only; a `testbox` alias
with non-default `IdentityFile` and `UserKnownHostsFile`; a two-commit git repo
and a clone standing in for the remote). No `known_hosts` was pre-seeded and no
key was copied to a default path at any point.

Automated: `cargo fmt`, `cargo clippy --all-targets` (clean), and the
orchestrator / i18n / dropdown / settings / widget test selection.

**Two tests in `e2e::keybinding_editor` are flaky under plain `cargo test`** and
should not be mistaken for fallout from this branch:
`test_unsaved_changes_confirm_dialog` and `test_confirm_dialog_discard` each
failed on one run of a broad filter and passed on the next, with a *different*
one of the pair failing each time; both pass in isolation. They belong to the
keybinding editor, which nothing here touches. `cargo test` runs the whole suite
in one process (see CONTRIBUTING on `autotests = false`), so order-dependent
pairs surface there where `cargo nextest` — which gives each test its own
process — would not. Noted rather than chased. One new
regression test, `a_config_alias_is_handed_to_ssh_as_the_alias`, covers F2
through a PATH shim (`tests/fixtures/fake-ssh-alias-only`) that answers for the
alias and refuses the resolved target; it hangs in `wait_until` without the fix
and passes with it.

That "hangs without the fix" property is exactly what made it fail on Windows
CI, and the gate was missed when the test was written. The shim is a `#!/bin/sh`
script standing in for `ssh` on `$PATH`; Windows cannot execute it, so the probe
never answers and the wait runs until the harness kills it at 180s — the same
symptom as a genuine regression, on a platform where the test can never be
meaningful. Every other shim-driven test here already carries
`#![cfg(all(target_os = "linux", feature = "plugins"))]`
(`orchestrator_pending_ssh.rs`, the `dormant_ssh` reproducers); this one lives in
a file whose other tests are Windows-safe path-completion coverage, so the gate
is on the function.

**Not covered by tests:** the host-key trust dialog itself. It needs a real
`ssh`, `ssh-keyscan` and `ssh-keygen` plus an unknown host key, which the existing
shim fixtures do not model; it was verified by hand instead, end to end, several
times (accept, cancel, and retry-after-cancel). A fixture that can present a
genuine unknown host key would be the right next piece of work.

---

# Round 2 (cold comparative study) — findings and outcomes

**Branch:** `claude/orchestrator-ux-round-2`, from `d9a99a1`.
**Source:** `docs/internal/workspace-sessions-comparative-study-cold.md` on
`claude/herdr-0.9-cold-comparative-study`, which compared this dock against
`herdr` 0.9.0 under a cold first-use protocol.

Everything below was re-tested by driving the real TUI in tmux at 200x50 against
the study's own fixture — `sshd` on `127.0.0.1:2222`, pubkey only, reached
through a `testbox` alias whose `IdentityFile` **and** `UserKnownHostsFile` are
both at non-default paths, with `known_hosts` deleted and the editor's state
directories removed before each run. Screens were captured whole, never as a row
range: two of the study's own conclusions turn out to be artefacts of partial or
colour-stripped captures, so that discipline is load-bearing.

## Scoreboard

| # | Sev | Finding | Outcome |
|---|---|---|---|
| 1 | 4 | Unknown host key is a dead end | **Does not reproduce** — fixed on `d9a99a1` |
| 2 | 3 | Failed remote create presents as permanently in progress | **Partly reproduces** — fixed the real half |
| 3 | 3 | Work does not survive the editor closing | **Reproduces** — not fixed, see below |
| 4 | 3 | `+ New` is a form and names no target | Reproduces — not fixed, see below |
| 5 | 2 | Dead "Add machine…" entry | **Reproduces** — fixed |
| 6 | 2 | Dock loses focus on selection, disabling `F2` | **Reproduces** — not fixed, see below |
| 7 | 2 | Dock truncates the error; wording reads cut off | **Fixed** (wording); truncation already mitigated |
| 8a | 1 | Remote rows name the machine twice, the workspace never | **Reproduces** — fixed |
| 8b | 1 | Stale `terminals/` directory after delete | **Reproduces** — fixed |
| 8c | 1 | Two worktree path conventions | Reproduces — deliberately not changed, see below |
| 8d | 1 | Placeholder indistinguishable from a real value | **Does not reproduce** |

## 1 — unknown host key (severity 4): does not reproduce

The study ran against `da558dd`. The error it quotes — `host key not trusted —
nothing was connected to` — is a string added in `cf4712d`, i.e. it is the
message shown when the user **declines** the trust prompt. So round 2 met the
prompt and did not record it.

Driven cold, with no `known_hosts` at either path, the create shows:

```
┌ Unknown host key ─────────────────────────────────────────────────────────┐
│  This computer has never connected to testbox before.                     │
│                                                                           │
│    Connects to  127.0.0.1 port 2222                                       │
│    Host key    256 SHA256:f+LXdQJ+hFNpW6p6s+sqwA1+sYrDPGywfulu054qAjY (ED25519)
│                                                                           │
│  Trusting records the key in /root/sshtest/etc/known_hosts, and this host  │
│  connects without asking again.                                           │
│  Continue only if this fingerprint is the one you expect.                 │
│                                                                           │
│▸ [ Cancel ]  Esc    [ Trust and connect ]                                 │
└───────────────────────────────────────────────────────────────────────────┘
```

Host, key type and SHA256 fingerprint are all present, and the path named is the
fixture's **non-default** `UserKnownHostsFile`. Accepting produced a genuinely
remote session:

```
SSH_CONNECTION=[127.0.0.1 55074 127.0.0.1 2222]
# REMOTE-DEMO-REPO
/root/.fresh/worktrees/demo/demo-1
```

with a live `sshd: root@pts/2`. Afterwards the key is in
`/root/sshtest/etc/known_hosts` and `~/.ssh/known_hosts` **was never created** —
`~/.ssh/` still contains only `config`.

The study's two sub-claims fail with it: `Retry` does **not** re-run the same
command and fail identically — it re-opens the trust prompt; and nothing
requires the saved-machine dialog's SSH-options field.

## 2 — "permanently in progress" (severity 3): the real half, fixed

The terminal-failure half does not reproduce. After declining, the row changes to
the error state within a couple of seconds and stays there, and the dock's
failure panel shows the reason wrapped in full with `[ Retry ] [ Dismiss ]`.

What *does* reproduce is the state the study was actually looking at. While the
trust prompt waits for an answer, the row read `Adding wor…` — and for as long
as it waits there is, correctly, no `ssh` process, no worktree and no
`known_hosts`. Every observation in the study's item 2 is consistent with that:
they were polling the world from outside while a modal sat waiting for input.
The label was describing work that was not happening.

Fixed: the row now says `Waiting for you to confirm the host key…` for exactly
as long as the prompt is up, so the dock and the modal agree.

The other half of that item — "clicking other workspace rows did not switch
workspaces" — is the modal being modal. That is correct behaviour; it only read
as a freeze because the row claimed background work was under way.

## 5 — "Add machine…" (severity 2): fixed

Reproduces exactly: clicking it sets the field to the literal string "Add
machine…", reveals nothing, and reverts to `Local` on Tab. It is not quite dead —
it *arms*, and a further **Enter** opens the Add Machine dialog — but nothing
says so, and the obvious mouse gesture leaves a control that lies and then
silently undoes itself.

Removed from the Machine dropdown, along with the arming machinery it needed
(`machineAddArmed` / `commitMachineAdd` / `revertMachineAdd`). Two things next to
it already do the job better: the `~/.ssh/config` hosts above it, which need no
registration at all, and `Other host…` for one typed by hand. Registering a
machine keeps its own home in the dock's `⋯` → Machines and the
`Orchestrator: Machines` command.

## 7 — error wording (severity 2): fixed

`host key not trusted — nothing was connected to` did read as though cut off
mid-clause. It is now `host key not confirmed — nothing was connected`.

The truncation half was already mitigated on `d9a99a1`: the row still elides to
the dock width, but selecting it opens a panel directly beneath carrying the full
reason, wrapped, with the two recovery actions — not "only in the status bar".

## 8a, 8b: fixed

* Remote rows carried `ssh:testbox  testbox` — the machine twice, the workspace
  name never — so two sessions on one host were indistinguishable. The row now
  renders `⇅ alpha  testbox`: the workspace name, then the machine once, with
  the `⇅` facet already saying ssh.

  The first attempt at this spelled the label `alpha · ssh:testbox`, and CI
  caught what manual testing did not. The dock row is a single line about 39
  columns wide, and it renders the backend target *beside* the label — so the
  longer label put the machine back in twice and pushed the pending status
  (`Connecting…`) off the end. `ssh_submit_is_non_blocking_and_shows_connecting_row`
  waits for that word, and waited the full 180s: a workspace being created
  showed its name and its host but never what it was doing, which is a worse
  version of the "permanently in progress" defect this round set out to fix.

  Two changes. `remoteDetailSegs` drops the target segment when the label
  already contains it, so the duplication cannot come back by another route.
  And the label is the workspace name again: the part of the fix that actually
  answered the study is upstream, in the form handing over its generated
  default when the name field is left blank, so `o.name` is populated and there
  is a name to show. Spelling the target into the label as well was over-reach.

  The test asserted the literal old label `ssh:dead-host`. That string only
  existed *because* of the defect — it was the whole label when no name was
  given. The assertion now checks the facet glyph, the host and the status on
  one row, which is what the test is actually about.
* Deleting a workspace left `terminals/<encoded-root>/` behind, one dead
  directory per delete. `DeleteWorkspace` now removes it alongside the workspace
  record; both are keyed by the same root and die together.

## 8d — placeholder styling: does not reproduce

`Identity file: [~/.ssh/id_ed25519]` **is** styled distinctly — italic, grey
(`ESC[3m ESC[38;5;241m`) against white for a real value. The study read a
plain-text capture, which strips SGR. Same class of error as the two the study
itself records against its participant.

## What is not fixed, and why

### 6 — dock focus on selection (severity 2)

Reproduces: after clicking a live row, focus is in its terminal and bare `F2`
does nothing.

But the premise that `F2` is "the dock's only route to Rename / Archive / Delete"
is wrong. Two routes work, both verified:

* **right-click on the row** opens the menu directly, focus or no;
* **`Alt+O` then `F2`** — `Alt+O` is the dock's focus toggle, which the dock
  advertises by underlining the `O` in its own title.

The obvious fix — keep dock focus on selection — is precluded:
`click_on_focused_dock_row_dives_focus_into_session` is an explicit regression
test asserting the opposite, and its comment records that live-switch-without-dive
*was* the earlier bug. Reverting it to satisfy this finding would trade one
regression for another.

I tried the task's other option, hanging the row's actions off the always-visible
`⋯` button, and **reverted it**, because I could not open that menu from a real
mouse click in tmux at any column — and neither could the study, whose own
`fresh-04-dock-overflow-menu.txt` capture shows the menu absent after clicking
it. An in-process e2e test (`dock_dropdown_mouse::open_dock_menu`) clicks the
same glyph and passes, so the two disagree. **That gap is a finding in its own
right** and wants its own investigation; building this fix on top of it would
have been shipping something I could not demonstrate.

What is genuinely missing is discoverability: neither working route is advertised
at the moment of need. That is a smaller change than either option in the brief,
and it should be made once the `⋯` question is settled.

### 3 — persistence across editor exit (severity 3)

Reproduces exactly. A probe writing a timestamp every second froze at `19:42:18`,
the second of `Ctrl+Q`, and had not advanced 20 s later; no process survived.

Not attempted. The brief anticipates this: running orchestrator sessions under
the existing daemon is not a setting but a change of process ownership. Today the
work is a child of the editor; `attachRemoteAgent`, the terminal manager, window
lifecycle and restart recovery all assume that. Putting a daemon in between means
deciding what happens when the daemon and the editor disagree about a session's
existence, how restart recovery reattaches rather than respawns, and what the
dock shows for a session whose daemon is gone — the study's own §6.2 shows the
reference product does not survive that either. It is the right thing to do and
it is the largest item here; it should not be guessed at inside a round of UX
fixes.

### 4 — `+ New` is a form (severity 3)

Reproduces: 2 clicks and a six-field modal, versus 1 click for the reference
product. The button is also context-free where the reference product's names its
target machine.

Not attempted, for the reason the brief allows: it changes the primary flow's
default. "Create immediately with the dock's defaults" needs an answer to what
the defaults *are* when the dock's selection is a remote workspace (create there,
or locally?), what happens when the current project is not a git repository, and
where the worktree/branch decisions go when nothing asked for them. The label
half (`[ + New · testbox ]`) is small and safe on its own, but shipping it
without the behaviour it is meant to describe would leave the button naming a
target that a dialog then asks about again.

### 8c — two worktree path conventions

Reproduces: local worktrees land in `~/.local/share/fresh/orchestrator/<slug>/<name>`,
remote ones in `~/.fresh/worktrees/<repo>/<name>`.

Deliberately unchanged. They differ because the machines differ: the local path is
inside the editor's own XDG data directory, and the remote host has no fresh
installation and therefore no such directory — `~/.fresh/worktrees/` exists
precisely because the remote side has no data dir to nest under. Unifying means
either inventing an XDG-shaped path on a machine with no fresh install, or moving
every existing local worktree, which are recorded in git's own worktree metadata
and in session records. Neither is a cleanup. Worth revisiting as an explicit
migration, not as part of this round.

## Investigated before touching: what the `fresh/fresh-sessions` push does

The study could not determine what creates the branch, and the delete dialog on
`d9a99a1` states it is "pushed to origin" — a claim never exercised, because the
fixture had no `origin`. I added one (a bare repo on disk) and ran the path.

**What happens.** Creating a workspace pushes nothing. **Deleting** one (and
archiving, and unarchiving — any lifecycle action that mutates the archive
manifest) creates a worktree at `<data dir>/orchestrator/.sync-workspace` on a
branch `fresh/fresh-sessions` inside the user's repository, commits `sessions.json`
as "Update sessions" **under the user's own git identity**, and pushes that branch
to `origin`. Confirmed: after one delete, `origin` grew
`refs/heads/fresh/fresh-sessions`.

**What it sends:**

```json
{
  "version": 1,
  "machine_id": "unknown",
  "updated_at": "2026-09-15T19:17:23.401Z",
  "active": [
    { "label": "demo", "branch": "demo", "base_ref": "origin/master",
      "created_at": "2026-09-15T19:16:13.397Z" }
  ],
  "archived": []
}
```

**How it fails.** Pointed at an unreachable `origin`, the delete still reports
`deleted 1 workspace(s)` and nothing on screen mentions the push at all. The
failure is silent; the branch and worktree remain locally.

**Judgement: it should not push by default.** Proposed first, as instructed;
the proposal was accepted and the push is now **removed** (see "Resolution"
below). The reasoning:

1. A local delete is not a network operation. Nothing in the flow suggests one,
   and the user is given no chance to decline before their remote is written to.
2. The branch lands in a **shared** namespace. On a team repository
   `fresh/fresh-sessions` appears for every collaborator and can trip CI, branch
   protection and webhooks — none of which the deleting user intended.
3. The commit is authored with the user's identity, so it reads as their work.
4. Failure is silent in both directions: offline users get no signal, and users
   who *do* have push rights get a branch they never asked for.
5. Cross-machine recovery is a real feature, but it is opt-in by nature — it only
   helps someone who works on two machines, and it should be their choice to
   sync a session list to a remote.
6. Nothing reads the branch back. Grepping the plugin for a fetch or a read of
   `refs/heads/<user>/fresh-sessions` finds none: cross-machine recovery is
   documented as "designed but deferred" (`orchestrator-sessions.md:406`). So
   the push had no consumer at all — it was a write-only side effect.

Proposed: keep the local branch and worktree (that is what makes recovery
possible at all, and it is now disclosed in the archive/delete confirmations),
and make the **push** opt-in — a setting, or a prompt on first use naming the
remote and the branch.

### Resolution

The push is removed. `syncSessions` still creates the `.sync-workspace`
worktree, still commits `sessions.json` to the local `<user>/fresh-sessions`
branch — so the snapshot a future recovery feature needs is still being kept —
and simply stops there. No setting was added: "don't write to a shared remote
when someone deletes a local row" is the correct default, not a preference, and
a setting for it would be one more thing to explain for a feature that has no
reader yet. When cross-machine recovery is actually built, the push belongs
with it, as something the user turns on and can watch fail.

The confirmation line changed with it, in all 15 locales:

```diff
-  • update fresh/fresh-sessions — Fresh's own session list, pushed to origin
+  • update fresh/fresh-sessions — Fresh's own session list, kept on this machine
```

Covered by `deleting_a_workspace_does_not_push_anything_to_origin`
(`orchestrator_attach_worktree.rs`), which adds a real bare `origin` to the
fixture — the thing the original study lacked — deletes two workspaces, waits
for the sync to finish, and asserts `origin` still has no refs.

The waiting is the fiddly part, and the first version of the test got it wrong:
it waited only for the local snapshot branch, which is committed *during* the
sync, and then checked `origin` — winning a race against the push that used to
follow. That version passed against the pushing code, which is to say it proved
nothing. The condition is now "the branch exists **and** the footer's `↻` is
gone", and `↻` shows for exactly as long as the sync is in flight, so it can
only become true after the push would have run. Verified both ways: with the
push put back the test fails with
`refs there are now: ["refs/heads/fresh/fresh-sessions"]`, and passes with it
removed.
