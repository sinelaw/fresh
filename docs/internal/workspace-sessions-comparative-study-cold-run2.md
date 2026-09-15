# Machine- and session-creation flows: `fresh` Orchestrator dock vs `herdr` 0.9

A cold, first-use comparative benchmark usability study (Nielsen Norman Group discount protocol,
single participant, think-aloud replaced by a full action/-screen log).

> **Second, independent run.** This is a separate cold study from
> [`workspace-sessions-comparative-study-cold.md`](workspace-sessions-comparative-study-cold.md),
> conducted without reading that report or its evidence. Same products, same task script, a
> freshly rebuilt fixture, and a participant equally ignorant of both products. It is kept
> alongside the earlier run rather than replacing it, so the two can be compared: agreement
> between two blind runs is stronger evidence than either alone, and any disagreement is worth
> investigating. Evidence for *this* run lives in `comparative-study-evidence-cold-run2/`.
> One difference in protocol: at the participant's direction `herdr` was run first here.

---

## 1. Method

**Persona.** First-time user of both products. Comfortable with git and ssh in general; knows
nothing about either product's concepts, vocabulary, or architecture. No documentation, no source,
no demos, no videos.

**Protection of ignorance.** Nothing in the `fresh` checkout was read: no `crates/`, `docs/`,
`README`, `CHANGELOG`, no `git log`/`git show`, no design notes, no prior reports. The repository
was used only to run `cargo build`. `herdr`'s source, its GitHub repository and `herdr.dev/docs`
were likewise not read — including the doc URLs that `herdr`'s own `--help` prints for AI agents,
which were deliberately not fetched. Both products were judged only on what they print or draw:
`--help`/`--version` output, menus, dialogs, key legends, status bars, and observable side effects
on the filesystem and process table.

**What "an action" means.** One mouse click, or one keystroke. Typed strings are counted as their
character count. Mis-aimed clicks caused by arithmetic errors in my own tmux/SGR harness are
counted separately and never charged to a product; every instance is called out in §8.

**Driving.** Each product ran in its own `tmux` session at 200x50. Input was sent with
`tmux send-keys`, screens read with `tmux capture-pane -p` (and `-e` when colour mattered).
Mouse clicks were synthesised as SGR press/release pairs.

**Order and its bias.** `herdr` was run first, `fresh` second, at the user's direction. With one
participant there is no counterbalancing, so a learning effect runs in `fresh`'s favour: by the time
I reached `fresh` I already understood the shared concepts (a machine, a workspace, a persistent
session). Where `fresh` nonetheless lost, the priming makes the result stronger, not weaker; where
`fresh` won, priming is not a plausible explanation because the wins are structural (auto-discovery,
an in-product registry) rather than fluency-based.

### Versions

| | |
|---|---|
| `fresh --version` | `fresh 0.5.1` |
| built from commit | `221657bd40548f97b76a815d2e635401053f09d2` (branch `claude/herdr-0.9-cold-comparative-study`) |
| `herdr --version` | `herdr 0.9.0` (installed via `https://herdr.dev/install.sh`) |
| host | Arch Linux, kernel 7.1.11, OpenSSH 10.5p1, tmux 3.7c |

### Fixtures

A private sshd on loopback, deliberately hostile to products that ignore ssh configuration:

```
Host testbox
  HostName 127.0.0.1
  Port 2222
  User noam
  IdentityFile      /home/noam/study/sshtest/etc/testkey          # non-default path
  UserKnownHostsFile /home/noam/study/sshtest/etc/known_hosts     # non-default path
```

Password authentication is disabled and the key lives at a non-default path, so **any successful
connection proves both directives were honoured**; a product that ignores them cannot connect at
all. `testbox` had never been connected to. Both `known_hosts` files were deleted between the two
product runs, state directories were wiped, and the git fixtures were rebuilt from scratch, so both
products started from identical conditions.

Two git fixture repos with two commits and three branches each (`main`, `feature/alpha`,
`bugfix/beta`), distinguishable at a glance by their README heading: `# LOCAL PROJECT` and
`# REMOTE PROJECT (testbox)`.

**Remoteness was never inferred from the shell prompt.** Because the "remote" host is loopback it
has the same hostname as the local machine. Every remote claim in this report is backed by
`echo $SSH_CONNECTION` inside the session plus a live `sshd-session: noam@notty` in the process
table.

### Deviations from the requested setup, and why

- The environment is Arch, not Debian, and the session user is `noam`, not `root`. `apt-get` does
  not exist; `openssh` and `tmux` were already installed. All `/root/...` paths became
  `/home/noam/study/...` and sshd runs as `noam` on port 2222.
- OpenSSH 10.5 resolves `~/.ssh/config` from the passwd database, **not** from `$HOME` (verified:
  with `HOME` overridden, `ssh -G testbox` returned `hostname testbox`, `port 22` — the block was
  not read). An isolated `HOME` therefore could not deliver the fixture. The `Host testbox` block
  was appended to the real `/home/noam/.ssh/config` with the user's explicit permission, after
  backing the file up; it is removed again in §9. The user's real `~/.ssh/known_hosts` was **never**
  modified — verified by md5 at every step — because the fixture points `UserKnownHostsFile`
  elsewhere.
- Both products were run under an isolated `HOME` so their state directories would be clean. Both
  turned out to resolve *some* of their state from the real home regardless (§8), which is noted
  as a measurement caveat rather than charged to either product.
- `fresh` was additionally run with `XDG_CONFIG_HOME` pointed at the study directory. This matters:
  with the default config path, `fresh` loaded the user's real `~/.config/fresh` — their `init.ts`,
  plugins and theme — which is *not* stock `fresh`. The first `fresh` run was contaminated this way
  and was discarded and redone. All `fresh` results below are from the stock-config run.

---

## 2. Task-by-task comparison

Times are wall-clock for the product's own work; my scripted `sleep`s are excluded where they could
be separated, and flagged where they could not.

| # | Task | `herdr` 0.9.0 | `fresh` 0.5.1 |
|---|---|---|---|
| 1 | **Register machine `testbox`** | ✅ CLI only. `herdr machine add testbox --label testbox`, then `yes` to the host-key prompt. ~43 keystrokes + 1 failed attempt (arg order). ~20 s, which includes bootstrapping a server on the remote. No TUI path exists. | ✅ **0 actions — already registered.** `testbox` was auto-discovered from `~/.ssh/config` and listed before I did anything. Opening the registry: `Ctrl+P` → `Orchestrator: Machines` → Enter (~24 keystrokes). |
| 2 | **Session on local machine** | ✅ **1 click** on `new`. Instant, no dialog. | ✅ Modal dialog. Minimum ~2–4 actions (`[ + New ]`, retarget machine, `[ Create Workspace ]`); my run used more because I also typed a path and name. 13 s. |
| 3 | **Session on remote `testbox`** | ✅ **2 clicks** (click the `testbox` group header — which retargets the button to `new · testbox` — then `new`). ~3 s. Verified remote: `SSH_CONNECTION=127.0.0.1 51082 127.0.0.1 2222`. | ❌ **Failed cold, 3 attempts.** `Host key verification failed.` Succeeded only after I added a second ssh-config block *outside the product* (§3.1). Then ~8 actions, 18 s. Verified remote: `SSH_CONNECTION=127.0.0.1 53176 127.0.0.1 2222`, README read back as `# REMOTE PROJECT (testbox)`. |
| 4 | **Return to a session** | ✅ 1 click away, 1 click back. Instant. Full scrollback preserved. | ✅ 1 click. ~3 s. Full scrollback preserved. Status bar correctly flips to `testbox`. |
| 5 | **Persistence across client quit** | ✅ **Pass by default.** Probe advanced `17:23:35` → `17:24:01` across the client being killed. Servers survived; reattach restored the exact terminal, probe still advancing (66→74 lines). | ⚠️ **Depends on mode.** Default foreground launch: probe **froze at `17:48:59`, the instant of quit**; no process survived. Daemon mode (`--cmd daemon new studyd` + `-a studyd`): probe advanced `17:53:04` → `17:53:29`, reattach restored the exact terminal, probe still advancing (66→72). **Parity — but only if you know daemon mode exists.** |
| 6 | **Second session, same machine + switch** | ✅ 1 click to create, 1 click each way to switch. Instant. Distinct scrollbacks. | ✅ ~8 actions to create (dialog), 16 s; 1 click each way to switch, ~3 s. Distinct scrollbacks. |
| 7 | **Tear down a session** | ✅ Dialog stated exactly what it would do; behaviour matched exactly (§4.8). | ⚠️ Local workspace: worktree removed as promised, branch retained. **Remote workspace: the promised `git worktree remove` did not happen** — worktree, directory and branch all left on the remote (§3.4). |

**Errors hit, and whether recovery was possible without leaving the product**

| Product | Error | Recovery inside the product? |
|---|---|---|
| `herdr` | `herdr machine add --label testbox testbox` printed only a usage line | Yes — reorder the arguments. |
| `herdr` | (non-TTY only, my harness) `ssh_askpass ... Host key verification failed.; machine was not saved` | Yes — rerun with a terminal; the prompt then appears. |
| `fresh` | `Host key verification failed.` on workspace creation | **No.** The dock offers no way to inspect or accept a host key. I recovered by running `ssh testbox` in `fresh`'s *own built-in terminal* and answering `yes` — which is technically "inside the product", but requires the user to already know that the bare `✗` means an untrusted host key. Even that was not sufficient (§3.1). |
| `fresh` | `✗` on **Test connection** | No explanation, no fingerprint, no detail anywhere on screen or in the status bar. |

---

## 3. Where `herdr` is better, and precisely why

Each item names the interaction and the moment, and is written to be implementable.

### 3.1 The unknown host key — the single decisive difference · **Severity 4 (catastrophe)**

**The moment.** `herdr machine add testbox --label testbox`, ~2 seconds in, prints:

```
The authenticity of host '[127.0.0.1]:2222 ([127.0.0.1]:2222)' can't be established.
ED25519 key fingerprint is: SHA256:yilIxq3ROom5tMCctLuBDqRNzVNRBbbI0whRMLVi1sg
This key is not known by any other names.
Are you sure you want to continue connecting (yes/no/[fingerprint])?
```

`herdr` does not reimplement trust-on-first-use. It invokes `ssh` **with the alias** and lets
OpenSSH's own prompt through to the terminal, so the user sees the real fingerprint and decides.
On `yes`, the key is written to the path the config actually specifies —
`/home/noam/study/sshtest/etc/known_hosts` — and the user's real `~/.ssh/known_hosts` is untouched.
Every subsequent connection uses `-o StrictHostKeyChecking=yes`: TOFU exactly once, strict
thereafter.

**The contrast.** `fresh`'s Machines dialog offers a **Test connection** button. Pressed against the
same host it renders one glyph — `✗ just now` — with no fingerprint, no error text, no status-bar
message, and no way to accept the key. Creating a workspace fails with
`Orchestrator: Host key verification failed.`

**The root cause, isolated.** I captured the process table while driving the dock. `fresh` issues
**two different ssh invocations for the same machine**:

```
Test connection:     ssh -o BatchMode=yes -o ConnectTimeout=8  -- testbox            uname -sr; ...
Create Workspace:    ssh -o BatchMode=yes -o ConnectTimeout=20 -p 2222 -- noam@127.0.0.1  cd '...' ...
```

The creation path **pre-resolves the alias** into `user@host` plus `-p`. OpenSSH matches `Host`
patterns against the *host argument*, and `127.0.0.1` does not match `testbox` — so the entire
`Host testbox` block is skipped and `IdentityFile`, `UserKnownHostsFile`, `ProxyJump`,
`IdentitiesOnly`, `Match` and everything else silently evaporate.

This is why accepting the key in `fresh`'s own terminal did **not** fix workspace creation: the key
landed in the configured `known_hosts`, which the creation path no longer reads. Adding a second
block —

```
Host 127.0.0.1
  IdentityFile       /home/noam/study/sshtest/etc/testkey
  UserKnownHostsFile /home/noam/study/sshtest/etc/known_hosts
```

— made the *resolved* form connectable, and workspace creation then succeeded immediately. So
`fresh`'s remote-workspace machinery is sound; only the alias pre-resolution is broken.

Two independently damaging consequences:

- **Severity 4** — remote workspaces cannot work for any host whose `~/.ssh/config` carries
  per-Host directives. That is the normal case for jump hosts, per-host keys, non-standard ports
  with dedicated keys, and hardware tokens.
- **Severity 3** — **Test connection actively misleads.** It exercises a code path with *different
  ssh-config fidelity* from the one that opens a workspace, so it reports `✓ ok` for a machine on
  which workspace creation cannot succeed. It converts a five-second diagnosis into an
  indefinite one, because the one diagnostic the product offers says the opposite of the truth.
  I verified this precise sequence: `✓ ok`, then `Host key verification failed.`, minutes apart, on
  the same machine.

*Adopt:* pass the alias to `ssh` and let OpenSSH resolve it; if a resolved tuple is needed for
display, resolve with `ssh -G` for display only. Make **Test connection** run byte-identical
arguments to the creation path. Add an in-product TOFU step: on `Host key verification failed`,
show the fingerprint with **Trust** / **Cancel**, and on Trust write to the `UserKnownHostsFile`
that `ssh -G` reports for that alias.

### 3.2 Persistence is the default, not a mode · **Severity 3**

`herdr` runs a server. You quit the client, the work keeps running; you reattach, the terminal comes
back with its scrollback and the loop still ticking. I never chose this, configured it, or knew it
existed — it is simply what happened. Reattach showed no welcome dialog and dropped me straight back
into the remote workspace.

`fresh` can do exactly this, and does it well — **in daemon mode**. `fresh --cmd daemon new studyd`
then `fresh -a studyd` gives a `fresh --server --session-name studyd` process that survives the
client, keeps the remote shells alive, and restores the exact terminal on reattach, running probe
and all. That is full parity.

But the **default** `fresh` invocation is not persistent: killing the client froze the probe at the
instant of quit and left no surviving process. On relaunch the session *records* came back and
reopening reconnected to the right remote worktree — but the scrollback was empty and the background
work was gone. The Orchestrator dock, which is the surface where a user thinks about long-running
remote sessions, contains no hint that daemon mode exists, that it is what makes work survive, or
which mode the current window is in.

This is a **defaults** problem, not a capability gap.

*Adopt:* make the daemon the default path for orchestrator workspaces, or — smaller change — show
the current mode in the dock and offer one action to promote the window to a daemon. The failure is
silent and destroys work, which is what earns severity 3 despite the capability existing.

### 3.3 Creation is one click, and the target is always visible · **Severity 2**

In `herdr` the button reads **`new · Local`**. Click the `testbox` group header and it becomes
**`new · testbox`**, and the pane switches to that machine. One mental model: *select a machine,
then `new` creates there.* Creating a session is one click, with no dialog, and the thing it is
about to do is written on the button before you press it.

`fresh` opens a modal **ORCHESTRATOR :: New Workspace** dialog with five fields (Launch in, Machine,
Project Path, Workspace Name, Agent) plus a worktree checkbox. It is more capable, and for a
first-remote-workspace it is arguably the right amount of ceremony — but it is the *only* path, so
the common case (another session, same place) costs a modal round-trip. A second, smaller friction:
the Machine dropdown defaults to **the last machine used**, not to Local or to the current
workspace's machine, so after making a remote workspace the next `[ + New ]` silently targets
`testbox`.

*Adopt:* give the dock a one-click "new workspace here" that inherits the selected workspace's
machine and path, and label it with its target the way `herdr` does. Keep the full dialog behind
`[ + New ]`. Default the Machine field to the selected workspace's machine, not the last used.

### 3.4 A destructive dialog that tells the exact truth · **Severity 3**

`herdr`'s worktree deletion dialog:

```
delete worktree checkout?
This removes the checkout folder:
/home/noam/.herdr/worktrees/local-proj/worktree-silver-cloud-8e4f
The branch is not deleted. The Herdr workspace will close.
```

It names the exact path it will remove, states what it will **not** touch, and names the side
effect. I checked afterwards: worktree deregistered, directory gone, branch
`worktree/silver-cloud-8e4f` still present, workspace closed. Promise and behaviour matched
precisely.

`fresh`'s dialog is in some ways *better written* — it enumerates three consequences and adds
"Uncommitted changes will be lost." (§4.3). But for a **remote** workspace it does not do what it
says. After `[ Confirm Delete ]` on `remote-two`:

- ✅ processes stopped (the probe froze)
- ✅ workspace record dropped from the dock
- ❌ `git worktree remove` **did not happen** — `/home/noam/.fresh/worktrees/remote-proj/remote-two`
  is still a registered worktree, the directory still exists, and branch `remote-two` remains

The same dialog on a **local** workspace behaved correctly (worktree removed, branch retained). So
the promise is kept locally and broken remotely, which is the worst shape for trust: the user learns
the dialog is honest, then it silently isn't.

*Adopt:* make remote deletion actually run `git worktree remove` over the connection — or, if it
cannot, say so in the result rather than in the promise. Also state the branch's fate explicitly, as
`herdr` does.

### 3.5 Failure leaves no half-built state · **Severity 2**

When `herdr machine add` could not verify the host key it printed
`Host key verification failed.; machine was not saved` and `herdr machine list` stayed empty. The
operation is atomic: prepare the remote, then save, or save nothing.

`fresh` created a **dock entry per failed attempt**. Three failed creations left three rows reading
`! ⇅ ssh:testbox  testbox  Host key v…`, cluttering the only surface the user is working in. They
were cleaned up on the next restart — but during the session, the moment you most need a clear view
is the moment the dock fills with corpses.

*Adopt:* do not persist a workspace record for a workspace that never opened. Report the failure as
a transient message; if a record is kept deliberately, give the row a one-click retry and dismiss.

### 3.6 Machine registration bootstraps the far side · **Severity 2**

`herdr machine add` ends with `Saved SSH machine <id>. Remote server is ready. Open Herdr clients
connect automatically.` In ~20 seconds it detected the remote platform, installed/prepared a server
there, saved the machine, and told me that already-open clients would pick it up — which they did:
the running TUI's sidebar re-shaped itself live, from a flat `spaces` list into a `machines` tree
with `▾ Local` and `▾ testbox ●`.

This is squarely `herdr`'s remit and arguably outside `fresh`'s (§5), so I am not scoring `fresh`
against it. It is listed because the *feedback* pattern is adoptable independently of the
bootstrapping: state plainly what was done, what is ready, and what will happen next.

### 3.7 Smaller wins worth copying

- **Prefix-mode hint bar** (severity 1). Pressing `ctrl+b` paints
  `PREFIX  esc cancel  ctrl+b send prefix  w workspace nav  ? keybinds` along the bottom. The
  modal state announces itself and its exits.
- **Session navigator** (`prefix+g`, severity 1): a searchable workspace → tab → pane tree showing
  each pane's working directory with `◆` on the current one. `fresh`'s `Orchestrator: Open` is a
  workspace-level selector; `herdr`'s goes one level deeper, to where the work actually is.
- **Connection state in the tree** (severity 1): a green `●` on the machine header, always visible.
  `fresh` shows liveness only as a per-row badge and in the status bar of the focused workspace.

---

## 4. Where `fresh` is better

`herdr` is the reference, not the winner by definition. These are real, and several are things
`herdr` simply does not do.

### 4.1 Machines are discovered from `~/.ssh/config` — registration is *zero* actions · **big win**

The single best moment in either product. I opened `Orchestrator: Machines` expecting to register
`testbox`, and it was **already there**:

```
Local             this computer                             1 workspaces
testbox      ssh  noam@127.0.0.1:2222           —           ~/.ssh/config
```

Resolved target shown, and a provenance column naming where it came from. Task 1 cost zero actions
and zero concepts. `herdr` requires an explicit `herdr machine add` per host and has **no** path to
this at all — I checked the `menu`, the keybind dialog (filtering for both `machine` and `remote`
returned `no matching keybinds`), the workspace context menu, and right-click on `new` and on the
`spaces` header. Machine management in `herdr` is CLI-only and nothing on screen says so
(**severity 3 against `herdr`**: a first-time mouse-first user who never runs `--help` cannot find
it).

*`herdr` should:* parse `~/.ssh/config` and offer those hosts in the sidebar as candidate machines,
with the same provenance label, so `machine add` becomes confirmation rather than data entry.

### 4.2 An in-product machine registry with real affordances · **win**

`fresh`'s Machines dialog has `[ + Add machine… ]`, `[ Edit ]`, `[ Test connection ]`, `[ Remove ]`,
and `[ New workspace here ]`. The Add/Edit form is genuinely good: a `Kind` radio (SSH / Kubernetes),
a **Host dropdown populated from ssh-config aliases** that live-resolves underneath
(`↳ noam@127.0.0.1:2222`), plus Name, Target, Identity file, SSH options (`-J jump-host · -o
ProxyCommand=…`) and Default path. It shows the user the model.

`herdr` has none of this on screen. (The bitter irony is §3.1: the registry is better than `herdr`'s
and the machines in it cannot be used.)

### 4.3 The delete confirmation is better written · **win**

```
Delete workspace local-two?

This will:
  • stop all workspace processes
  • run `git worktree remove`
  • drop the workspace record

Uncommitted changes will be lost.

[ Cancel ]  [ Confirm Delete ]
```

Three enumerated consequences and an explicit data-loss warning beats `herdr`'s two-sentence
version. *`herdr` should* adopt the enumerated-consequences shape — and `fresh` should adopt
`herdr`'s one missing line, "The branch is not deleted", and then actually keep the promise
remotely (§3.4).

### 4.4 A persistent machine indicator in the status bar · **win**

Stock `fresh`'s status bar reads `Trusted  Local  Ln 1, Col 1` and flips to `Trusted  testbox` the
moment a remote workspace is focused, with transient messages like `Connected: remote-two`. "Which
machine am I about to type on" is ambient and always answerable. In `herdr` you infer it from which
sidebar group is highlighted. For an editor — where a stray command runs against whatever machine
owns the pane — this is the safer default. *`herdr` should* put the machine name in its own bottom
bar.

### 4.5 A searchable command palette with descriptions and provenance · **win**

`Ctrl+P` lists commands with a sentence of explanation and a source column (`builtin`,
`orchestrator`, `k8s-workspace`, `live_grep`, `init.ts`). Typing `orchestrator` surfaced the entire
feature in one screen, including `Orchestrator: Machines — Register and manage the machines
workspaces run on`, which is how I found Task 1 at all. `herdr`'s keybinds dialog is searchable but
lists *bindings*; unbound capabilities appear only as `unset`, and there is no description text.
*`herdr` should* make its command surface searchable by description, not only by key.

### 4.6 Self-explaining state · **win**

`Orchestrator: Explain State — Say why the selected workspace shows its state badge (rule, evidence,
timing)`. A command whose entire job is to explain the UI's own inference. I did not get to exercise
it (§7), but nothing in `herdr` occupies this space, and it is the right instinct for a dock that
shows status badges.

### 4.7 Contextual, honest form hints · **win**

The worktree checkbox re-explains itself as the form changes: `↳ Not checked — the host decides when
you create` for a remote path, `↳ disabled — non-git` once the path isn't a repository. The field
tells you why it is in the state it's in. (It is undercut by the checkbox *glyph* contradicting the
text — `[v]` above "Not checked" — see §6.)

---

## 5. Different, equally valid choices — noted, not scored

- **Scope.** `herdr` is a terminal workspace manager for coding agents; `fresh` is an editor with an
  orchestrator dock. `herdr` has no editor and is not marked down for it. `fresh` has no
  agent-integration registry, no tmux-style pane/tab algebra as a primary surface, and no
  remote-server bootstrapper, and is not marked down for those either. The comparison is confined to
  machine registration and session creation/return/persistence/teardown, where both genuinely
  compete.
- **Grouping.** `herdr` groups the sidebar *by machine* (`▾ Local`, `▾ testbox ●`). `fresh` keeps a
  flat list and tags remote rows inline (`· ⇅ ssh:testbox  testbox`). Grouping scales better with
  many machines; a flat list keeps recency ordering. Both are defensible.
- **Ceremony.** `herdr` creates instantly with generated defaults and lets you rename afterwards;
  `fresh` asks up front. Fewer decisions later versus fewer surprises later.
- **Modality.** `herdr` is tmux-shaped (`ctrl+b` prefix); `fresh` is application-shaped (menu bar,
  palette, direct keys). Each matches its host genre.
- **Worktrees.** `herdr` exposes worktree creation as its own command with a generated branch name
  (`worktree/silver-cloud-8e4f`) and its own checkout root; `fresh` folds it into the create dialog
  as a checkbox. Equivalent power, different entry points.
- **Naming.** `herdr` names machines with an explicit `--label`; `fresh` inherits the ssh alias.

---

## 6. Visual style, input and focus

Requested as a separate dimension; kept out of the scoring above except where it changes behaviour.

**Palette.** `herdr` renders in Catppuccin Mocha and uses it consistently: base `#1e1e2e` for panes,
mantle `#181825` for the sidebar and dialogs, text `#cdd6f4`, dimmed `#6c7086`/`#7f849c`, blue
`#89b4fa` for borders and selection, mauve `#cba6f7` for git branches, green `#a6e3a1` for the
connected `●`. Hue carries meaning: branch names are always mauve, liveness is always green.

Stock `fresh` uses saturated RGB primaries: pure black `#000000` ground, pure white `#ffffff` text,
pure yellow `#ffff00` for the active tab and a `▌` marker, pure cyan `#00ffff` for dock workspace
names and footer hint keys, cornflower `#6495ed`, selection `#0064c8`, and a grey ramp
(`#141414`, `#1e1e23`, `#323237`, `#464646`, `#7f7f7f`, `#8c8c8c`). Maximum-chroma yellow and cyan
on black is high-contrast but reads as terminal-default rather than designed, and the two accents
are close in luminance so they compete. The grey ramp is well-judged; the accents are the weak part.
*This is taste, not a defect* — but `herdr` looks deliberate at a glance and `fresh` does not.

**Selection.** `herdr` inverts a full block (blue background, dark bold text) so the highlighted item
is unmissable. `fresh` uses `#0064c8` behind the row — correct, slightly quieter.

**Chrome and density.** `herdr` is consistently lowercase (`machines`, `new`, `menu`, `agents`,
`settings`, `keybinds`) with no decorative icons beyond `·`, `▾`, `●`, `◆`, `└─`, one space of
padding on sidebar rows, and dialogs that are compact with a title row and a footer hint row
(`↵ create and open     esc cancel`). `fresh` mixes Title Case menus with lowercase hints and is
much sparser: the New Workspace dialog spans ~30 rows with three large vertical gaps between five
fields. Two visible layout artifacts in `fresh`: the Confirm Delete dialog draws a **nested double
frame** (an outer `┌─┐` around an inner `╭─╮`), and the outer frame is sized for the longest possible
content rather than the actual, leaving a wide empty margin on the right for short messages.

**Misleading affordances found.**
- `fresh`: the worktree checkbox renders `[v]` directly above the text `↳ Not checked — the host
  decides when you create`. Glyph and caption contradict each other (**severity 2**).
- `fresh`: the dock's `⋯` overflow control did not respond to clicks in any state I tried
  (**severity 2** — either wire it up or remove it).
- `herdr`: `new · Local` looks like two controls; the `· Local` half is not separately clickable and
  clicking it just fires `new` (**severity 1**). The label is genuinely useful, so the fix is to
  make it a real machine picker rather than to remove it.

**Focus management — the biggest interaction difference.**

`herdr` is explicitly mouse-first ("this is a mouse-first terminal") and every surface is also
keyboard-reachable; clicking a sidebar row both selects *and* activates it. Its one focus wrinkle is
that a context menu requires the row to be **selected first** — right-clicking an unselected row does
nothing, so it costs two actions where one would do (**severity 1**).

`fresh` separates focus from clicking in a way that cost me real time: **clicking the Orchestrator
dock does not focus it.** With the dock plainly visible and a row plainly clicked, `F2` (advertised
in the dock's own footer as `F2 menu`) did nothing, because keyboard focus was still in the editor.
Focus only moves via `Alt+O` / `Toggle Orchestrator Dock Focus`. The dock displays its key legend —
`↑↓ switch  →← fold` / `Enter edit  F2 menu` — *whether or not it has focus*, so the legend
advertises keys that will not work. And `Alt+O` is a toggle in both directions, so pressing it when
focus is already in the dock silently removes it (**severity 2**). *Adopt:* make a click in the dock
focus the dock, and dim the dock's key legend when it is unfocused.

---

## 7. Feature inventory (everything observed, including outside the task script)

Requested explicitly. Everything here was seen on screen or in the process/file system; nothing is
inferred from documentation.

### `herdr` 0.9.0
- First-run orientation dialog: mouse-first terminal, click sidebar to switch, drag pane borders to
  resize, right-click context menus, `ctrl+b` prefix, `?` for keybinds.
- First-run agent-integration installer probing 17 agent CLIs (`claude`, `codex`, `opencode`, `grok`
  available; `pi`, `omp`, `copilot`, `devin`, `droid`, `kimi`, `kilo`, `hermes`, `qodercli`, `qwen`,
  `cursor`, `mastracode`, `antigravity` not found), so agents "report state directly instead of
  relying only on process detection".
- Auto-creates a workspace from the launch cwd, labelled with repo name + current git branch.
- Sidebar header changes `spaces` → `machines` once a machine exists; groups by machine with a
  green `●` connection indicator.
- `new · <machine>` target-labelled create button; `menu` → settings / keybinds / reload config /
  detach.
- Settings tabs: theme, indicators, sound, toasts, integrations.
- Keybinds dialog: `/`-searchable, grouped global / navigation / workspaces+tabs / panes, showing
  `unset` for unbound commands.
- Prefix-mode hint bar; session navigator (`prefix+g`) with pane-level cwd and `◆` current marker.
- Workspace context menu: Rename / Close / New worktree / Open worktree…; worktree node menu adds
  Delete worktree checkout…. Worktrees nest under their parent (`└─ · silver-cloud…`).
- Tabs per workspace, splits (vertical/horizontal), zoom, resize mode, copy mode, edit scrollback,
  rename pane/tab/workspace, move tab left/right, workspace/tab switching by number.
- Named sessions (`--session`, `--remote-session`) producing separate servers and state under
  `~/.config/herdr/sessions/<name>/`.
- Socket/CLI API: `machine`, `workspace`, `worktree`, `tab`, `pane`, `agent`, `notification`,
  `session`, `integration`, `api`, `config`, `channel`, `server`, returning JSON.
- `herdr --skill` prints an agent skill file; help blocks address AI agents directly with doc URLs.
- Update channels (stable/preview), `herdr update`, `herdr status`, `server reload-config`.

### `fresh` 0.5.1 (Orchestrator-related and adjacent)
- Orchestrator dock, toggled from **View → ☐ Orchestrator Dock** (second item; no keybinding shown,
  unlike File Explorer's `Ctrl+B`). Open by default in stock config.
- Dock: `[ + New ]`, `/ search`, `⋯` overflow, workspace rows with `·` state dot, `⇅` remote marker,
  inline machine tag, live process/title (`· local-proj · bash — noam@chunky:/h…`), folder folding,
  footer key legend.
- Dock row menu (`F2`): Visit… / Rename… / Move to Folder… / Archive / Delete.
- Orchestrator commands: Explain State, Jump Back, Jump to Attention, Kill Selected, Machines,
  Move to Folder…, New Workspace, Open, Toggle Dock, plus builtin `Toggle Orchestrator Dock Focus`
  (`Alt+O`).
- Machines dialog with ssh-config auto-discovery and provenance column; Add/Edit form with
  SSH/Kubernetes kinds, alias dropdown with live resolution, Identity file, SSH options, Default
  path; Test connection; Remove.
- New Workspace dialog: Launch in, Machine, Project Path (with "blank = remote home"), Workspace
  Name, Agent (`terminal ▼`), worktree checkbox with contextual hints; `[ Create Workspace ]` and
  `[ Create in Background ]`.
- Creates git worktrees per workspace — locally under
  `~/.local/share/fresh/orchestrator/<slug>/<name>`, remotely under `~/.fresh/worktrees/<repo>/<name>`
  with a branch named after the workspace; remote side also keeps `~/.fresh/windows.json`.
- Daemon/session mode: `--cmd daemon list|attach|new|kill|info|open-file`, `-a/--attach`,
  `fresh --server --session-name <n>`; `daemon list` reports and cleans stale daemons.
- Integrated terminals: Open Terminal / Below / To the Right / in Utility Dock (`Alt+\``), Focus
  Terminal, Restart Terminal Process, Send Selection to Terminal, terminal scrollback search, tab
  titles tracking the live process (`bash —` → `ssh —`).
- Editor surface: menu bar (File/Edit/View/Selection/Go/LSP/Help), multi-mode palette
  (`file | >command | :line | #buffer`), splits, line numbers/wrap, scrollbars, themes, locales,
  background image/blend, page width, keyboard calibration, LSP menu, plugins (`k8s-workspace`,
  `live_grep`, `orchestrator`), `init.ts` scripting with typed API, `--safe` recovery mode.
- Status bar: `Trusted`, current machine, cursor position, transient messages, encoding/EOL/filetype.
- Unrequested side effect observed: a `.sync-workspace` worktree on branch `noam/fresh-sessions`
  appeared **inside the fixture project repo** during the session (§8).

---

## 8. Measurement caveats, and my own errors

Kept separate from product findings, as required.

**My harness errors — not charged to either product.**
- My SGR click helper sends press+release with no motion event. `herdr`'s context-menu items
  highlight on motion and act on the *highlighted* item, so clicking a menu entry never activated
  it. Keyboard navigation of the same menu worked immediately. I initially mistook this for a
  product defect and was wrong.
- I twice reported a `herdr` action as failing (`prefix+shift+D` close workspace) when my own
  prefix-mode sequencing had lapsed; on a clean retry it worked. `herdr`'s workspace close is fine.
- `prefix+c` / `prefix+v` (new tab, split) never registered through synthetic input even though
  `?`, `g`, `N`, `D`, `G` did. **I could not exercise `herdr`'s tabs and splits and do not claim
  they are broken.**
- I paged the keybind list with `PPage`+`NPage` in the same round, which cancelled out, and briefly
  concluded the list was short.
- I read `fresh`'s first palette screen with `head -20` and concluded `Ctrl+P` was dead; the palette
  was open, rendered lower down.
- A stray click of mine opened a second New Workspace dialog and my typed probe landed in its
  Project Path field.
- A heredoc of mine contained backticks and executed `fresh`, producing an unrelated init error.
- My `pkill -f 'herdr server'` matched its own command line and killed my shell (twice).

**Fixture limitations that weaken specific comparisons.**
- **The loopback fixture is degenerate for `herdr`.** Because "remote" is the same machine *and the
  same user*, `herdr machine add testbox` with the default session made the remote bridge reach the
  **same server**, and the sidebar then rendered the identical workspace set under both `Local` and
  `testbox` (`herdr workspace list` confirmed only 4 workspaces existed while 8 rows were drawn).
  This is a fixture artifact, not a `herdr` defect. I re-ran with `--remote-session remotebox` to
  force a genuinely separate remote server, and all `herdr` remote results above come from that
  configuration. A first-time user on a *real* second machine would not need the flag.
- **Neither product isolates cleanly under `$HOME`.** `herdr` always stores server/session state in
  the real `~/.config/herdr` while honouring `$HOME` for its machine store — which briefly made
  `herdr machine list` look empty and cost me a confused detour. `fresh` resolves Config from the
  real `~/.config/fresh` while honouring `$HOME` for Data and Logs, which silently loaded the user's
  personal `init.ts`, plugins and theme into what was supposed to be a stock first-run. I discarded
  that contaminated run and redid `fresh` with `XDG_CONFIG_HOME` isolated. Neither behaviour is a
  usability finding for the study's tasks, but both are real state-resolution inconsistencies.
- Timings include scripted `sleep`s wherever the product's own completion could not be observed
  precisely; those are marked and should be read as upper bounds. Action counts are exact.
- `fresh`'s remote worktrees were created under the **real** `/home/noam/.fresh` because the ssh
  session's home is the real one. Similarly a `.sync-workspace` worktree and a `noam/fresh-sessions`
  branch appeared inside the fixture repo. I did not determine which operation created them, so I
  report the observation and not a mechanism (**severity 2** as an unrequested write into a user's
  repository — a user would be surprised to find a new branch there).

---

## 9. Prioritised adoption list for `fresh`

Smallest set of changes closing the biggest gaps, favouring **changed defaults and removed
misleading affordances** over new settings and panels.

| # | Change | Kind | Sev | Why it's first |
|---|---|---|---|---|
| 1 | **Pass the ssh alias to `ssh`; stop pre-resolving to `user@host -p`.** Use `ssh -G` for display only. | Fix | 4 | Single cause of total remote failure. Restores `IdentityFile`, `UserKnownHostsFile`, `ProxyJump`, `Match` for free. Verified: once the resolved form was made connectable, everything downstream worked. |
| 2 | **Make Test connection use byte-identical arguments to workspace creation** — or delete the button. | Remove misleading affordance | 3 | Today it reports `✓ ok` for machines that cannot open a workspace. A diagnostic that lies is worse than none. |
| 3 | **Handle unknown host keys in-product**: on `Host key verification failed`, show the fingerprint with **Trust** / **Cancel**; on Trust, append to the `UserKnownHostsFile` that `ssh -G <alias>` reports. | Fix | 3 | The one thing `herdr` does at exactly the right moment. Turns a dead end into two seconds. |
| 4 | **Make persistence the default** for orchestrator workspaces (daemon-backed), or show the current mode in the dock with one action to promote. | Default | 3 | The capability already exists and works perfectly; only the default and its discoverability are wrong. Silent work loss. |
| 5 | **Make remote delete actually run `git worktree remove`**, and state the branch's fate in the dialog. | Fix | 3 | The dialog currently promises something it does not do remotely. |
| 6 | **Never show a bare `✗`.** Any failed connection surfaces the underlying `ssh` stderr in the row or on hover. | Remove misleading affordance | 3 | The whole diagnosis in §3.1 was invisible to a normal user. |
| 7 | **A click in the dock focuses the dock**; dim the dock's key legend when unfocused. | Default | 2 | The dock advertises `F2 menu` while `F2` does nothing. |
| 8 | **Don't persist dock rows for workspaces that never opened**; make failures transient with a retry. | Remove misleading affordance | 2 | Three corpses per three failures, in the primary work surface. |
| 9 | **Default the Machine dropdown to the selected workspace's machine**, not the last used; add a one-click "new here" that inherits machine + path. | Default | 2 | Removes a modal round-trip from the commonest action and a class of wrong-machine mistakes. |
| 10 | **Fix the worktree checkbox glyph/caption contradiction**; wire up or remove the dock's `⋯`. | Remove misleading affordance | 2 | Two controls that say things that aren't true. |
| 11 | **Don't create branches/worktrees inside the user's repo silently** (`noam/fresh-sessions`, `.sync-workspace`) — or name them in the UI. | Fix | 2 | Surprising writes into user-owned history. |
| 12 | Tighten the New Workspace dialog's vertical rhythm; fix the nested double frame and oversized outer frame on Confirm Delete. | Polish | 1 | Cheap, and the dialogs are the product's face during the tasks that matter. |

### And for `herdr`, from `fresh`

1. **Read `~/.ssh/config` and offer those hosts as candidate machines** (severity 3) — turn
   `machine add` into confirmation rather than data entry.
2. **Expose machine management in the TUI at all** (severity 3) — today a mouse-first user who never
   runs `--help` cannot find it.
3. `herdr machine add`'s help prints `[OPTIONS] --label <LABEL> <SSH_TARGET>` but the parser requires
   the target first; `--label x testbox` prints only a bare usage line (severity 2).
4. Adopt `fresh`'s enumerated-consequences delete dialog and its persistent machine indicator.
5. Let right-click open a context menu without requiring the row to be selected first (severity 1).

---

## 10. Honest statement of completion

**Completed in `herdr` (7/7):** machine registration; local session; remote session (verified remote
by `SSH_CONNECTION` and live `sshd-session`); return to a session; persistence (probe survived a
client kill and kept advancing); second session plus switching; teardown of both a workspace and a
worktree, with the promised and actual side effects compared.

**Completed in `fresh` (7/7, one only via an out-of-product workaround):** machine registration
(zero actions — auto-discovered); local session; **remote session only after adding a `Host
127.0.0.1` block to `~/.ssh/config` outside the product** — cold, from the state a first-time user
is in, this task **failed**; return to a session; persistence (failed in the default mode, passed in
daemon mode); second session plus switching; teardown of a local and a remote workspace.

**What I could not do, or could not verify.**
- I could not exercise `herdr`'s tabs or splits: `prefix+c` and `prefix+v` never registered through
  synthetic tmux input although other prefix keys did. I make no claim about them.
- I could not evaluate `herdr`'s remote behaviour on a genuinely separate host. The loopback fixture
  collapsed local and remote onto one server until I forced separation with `--remote-session`;
  everything reported is from the forced-separate configuration.
- I did not determine what created `fresh`'s `.sync-workspace` worktree and `noam/fresh-sessions`
  branch in the fixture repo. I report the artifact, not a mechanism.
- I did not exercise `fresh`'s `Orchestrator: Explain State`, `Jump to Attention`, `Archive`,
  `Move to Folder…`, agent kinds beyond `terminal`, or the Kubernetes machine kind; nor `herdr`'s
  agents panel (empty with no agents running), notifications, or update channels.
- `herdr update` fails in this environment because the GitHub API is blocked by the egress proxy.
  Expected, and irrelevant to these tasks.
- Action counts are exact; several wall-clock figures include scripted waits and are upper bounds.

**Where the comparison was weak.** Task 1 is close to incommensurable: `fresh` makes it free by
reading `~/.ssh/config`, while `herdr` requires a CLI command it never advertises on screen. Both
answers are defensible and I have scored the *discoverability* of each rather than pretending one
"won" the registration step. Task 5 was initially mis-measured on my part: I compared `herdr`'s
always-on server against `fresh`'s default foreground launch, which is not the right comparator. Re-run
against `fresh`'s daemon mode, the two are at parity, and the finding is correctly about defaults and
discoverability, not capability.

**The bottom line.** `fresh`'s Orchestrator has the better machine model on screen — auto-discovered
machines with provenance, a real registry, a better-written destructive dialog, an ambient machine
indicator, and a genuinely excellent command palette. It is let down by one defect with a
disproportionate blast radius: pre-resolving the ssh alias discards the user's ssh configuration on
the only code path that matters, and the product's own diagnostic button tests the *other* path and
says everything is fine. Fixing items 1–3 would, on the evidence of this study, move `fresh` from
"cannot open a remote workspace on a realistically configured host" to parity with the reference.

---

## 11. Raw evidence

All captures are under `docs/internal/comparative-study-evidence-cold-run2/` (82 files). The decisive ones:

| Moment | File |
|---|---|
| `fresh` runs two different ssh invocations (the core finding) | `fresh-46-ssh-invocation-divergence.txt` |
| `fresh` Machines dialog, `testbox` auto-discovered from ssh config | `fresh-11-machines-stock.txt` |
| `fresh` Test connection → bare `✗`, no detail | `fresh-12-test-connection-stock.txt` |
| `fresh` Test connection → `✓ ok` after in-terminal TOFU | `fresh-15-test-connection-after-tofu.txt` |
| `fresh` workspace creation → `Host key verification failed.` | `fresh-17-remote-workspace-created.txt` |
| `fresh` remote workspace working, remoteness proven | `fresh-19-remote-workspace-success.txt`, `fresh-20-remote-verified.txt` |
| `fresh` persistence, both modes | `fresh-35-persistence-corrected.txt` |
| `fresh` delete confirmation text | `fresh-39-delete-confirm.txt`, `fresh-43-local-delete-confirm.txt` |
| `fresh` teardown: local removed, remote left behind | `fresh-44-teardown-local-vs-remote.txt` |
| `herdr` host-key TOFU prompt at `machine add` | `herdr-A1-hostkey-tofu-prompt.txt` |
| `herdr` machine saved + remote server bootstrapped | `herdr-A2-machine-add-success.txt` |
| `herdr` sidebar re-shapes into machine groups | `herdr-11-sidebar-after-machine-add.txt` |
| `herdr` persistence result | `herdr-A10-persistence-result.txt` |
| `herdr` worktree delete promise, and the kept promise | `herdr-A19-worktree-delete-confirm.txt`, `herdr-A20-worktree-after-delete.txt` |
| `herdr` degenerate-loopback artifact (fixture caveat) | `herdr-12-degenerate-loopback-mirrored-list.txt` |
| Colour palettes | `herdr-A21-full-screen-colors.txt`, `fresh-45-full-screen-colors.txt` |
