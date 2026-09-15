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
manifest pushes the session list to `refs/heads/<user>/fresh-sessions`, which is
maintained through a worktree of its own at
`<data dir>/orchestrator/.sync-workspace` so it does not disturb the user's
`git worktree` set. It fires on archive, delete and unarchive — not on create,
which is why it does not show up if you only look after making a workspace.

The report asked for a one-line disclosure. It goes in the archive and delete
confirmations, for the same reason F9's line does: those dialogs already
enumerate what the action does, and a list that careful reads as "and nothing
else". Writing a branch into the user's repository and pushing it to origin is a
larger omission than the surviving branch was. The branch name in the line is
derived, not hard-coded, so it names the ref the user will actually find:

```
This will:
  • stop all workspace processes
  • run `git worktree remove`
  • drop the workspace record
  • keep the branch — `git worktree remove` does not delete it
  • update fresh/fresh-sessions — Fresh's own session list, pushed to origin
```

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

**Not covered by tests:** the host-key trust dialog itself. It needs a real
`ssh`, `ssh-keyscan` and `ssh-keygen` plus an unknown host key, which the existing
shim fixtures do not model; it was verified by hand instead, end to end, several
times (accept, cancel, and retry-after-cancel). A fixture that can present a
genuine unknown host key would be the right next piece of work.
