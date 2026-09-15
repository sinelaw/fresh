# Comparative usability study: machine- and session-creation flows
## `fresh` Orchestrator dock vs `herdr` 0.9.0 — cold, first-time-user protocol

**Date:** 2026-09-15
**Protocol:** Nielsen Norman Group discount usability testing, single participant, think-aloud
replaced by transcript capture. Researcher and participant are the same agent.

---

## 1. Method

### Persona and constraints

The participant was a first-time user of **both** products. Before and during the study the
participant did not read either product's source, documentation site, design notes, changelog,
commit history, or repository instruction files. The only permitted inputs were:

* build/install commands,
* `--help` and `--version` output,
* whatever each product drew on screen (menus, dialogs, key legends, status bars, hints),
* the state each product left on disk, inspected from outside the product.

This constraint is the instrument. Anything the participant could not work out from the running
product is recorded as a finding, not researched away.

### Products and versions

| | `fresh` | `herdr` |
|---|---|---|
| Version | `fresh 0.5.1` | `herdr 0.9.0` |
| Provenance | built from this checkout, `cargo build` → `target/debug/fresh` | `curl -fsSL https://herdr.dev/install.sh \| sh` → `/root/.local/bin/herdr` |
| Commit under test | `da558ddca021c56d92acfca0d9edf2ba105be43e` | n/a (release binary) |
| Feature under test | Orchestrator dock | workspaces / machines / sessions |

`herdr update` was not exercised; the GitHub API is blocked by this environment's egress proxy.
That did not affect the study — the installer delivered 0.9.0 directly.

### Scope difference (stated up front, and not scored)

`herdr` is a terminal workspace manager: persistent multi-machine sessions **are** the product.
`fresh` is an editor; the Orchestrator dock is one panel inside it. Nothing below penalises
`fresh` for lacking terminal-multiplexer features outside the dock's remit, or `herdr` for not
being an editor. What *is* compared is the job both products visibly advertise and both
implement: **create and return to workspace sessions on the local machine and on a remote SSH
machine.**

### Fixture

A local `sshd` on `127.0.0.1:2222` with its own host key, password authentication disabled, and
a client config at `~/.ssh/config`:

```
Host testbox
  HostName 127.0.0.1
  Port 2222
  User root
  IdentityFile /root/sshtest/etc/testkey
  UserKnownHostsFile /root/sshtest/etc/known_hosts
```

Both the identity file and the known-hosts file are at **non-default paths**, and `testbox` had
never been connected to. Because password auth is off and the key is not at `~/.ssh/id_ed25519`,
**any successful connection proves the product honoured both directives**, and a product that
ignores the `Host testbox` block cannot connect at all. This is the discriminating condition of
the study.

Git fixtures: `/root/work/demo` (README heading `# LOCAL-DEMO-REPO`) and `/root/remote-work/demo`
(`# REMOTE-DEMO-REPO`), each with 4 commits and branches `main`, `feature/alpha`, `bugfix/beta`.
The differing headings make local and remote distinguishable at a glance despite loopback.

**Between the two runs** the participant deleted both `known_hosts` files, removed each product's
state directories, restored both git fixtures from a pristine copy, and restarted `sshd` from the
prescribed config. `herdr` ran first; `fresh` ran second. Verified clean before the `fresh` run:
no `known_hosts` at either path, no `~/.config/herdr`, no `~/.herdr`, fixtures back to 4 commits
and 3 branches.

### Driving the TUIs

Both products ran in `tmux` at 200×50, driven with `tmux send-keys` and SGR mouse sequences, read
back with `tmux capture-pane -p -e`. **Mis-aimed clicks caused by the participant's own
coordinate arithmetic are counted separately and never charged to a product.** Two occurred; both
are called out in §7.

### Verifying remoteness

The "remote" host is loopback, so hostname and shell prompt prove nothing. A session was accepted
as genuinely remote only on `echo $SSH_CONNECTION` inside it, plus a live `sshd: root@…` process,
plus the `REMOTE-DEMO-REPO` README heading.

---

## 2. Task-by-task comparison

Action counts are clicks + keystroke-groups the participant issued. Wall-clock is measured but
noisy: the harness inserts fixed sleeps between synthetic input events, so **action count is the
reliable metric and time is indicative only.** Where a product's own latency was measured
cleanly, it is stated separately.

| # | Task | `fresh` | `herdr` |
|---|---|---|---|
| 1 | Register machine `testbox` | ✅ **0 dedicated actions** — already in the Machine dropdown, auto-discovered from `~/.ssh/config`. Explicit "Add machine…" entry is dead (§4.1). | ⚠️ **Succeeded, CLI only.** ~107 s of failed TUI hunting, then `herdr machine add`. 1 usage error, then success. Command itself: **1.1 s** once the host key is known. |
| 2 | Create session on local machine | ✅ 2 clicks (`[ + New ]`, `[ Create Workspace ]`), ~4 s. Creates a git worktree + branch `demo-1`. | ✅ **1 click** (`new`), workspace present in **~0.35 s**, no dialog. |
| 3 | Create session on remote `testbox` | ❌ **FAILED** — `Host key verification failed.` 4 clicks. Only in-product recovery is "Retry", which fails identically. Root cause proven in §4.2. | ⚠️ **Flow succeeded in 2 clicks (~3.2 s); remoteness UNVERIFIED** — see §6.1. |
| 4 | Return to a session after navigating away | ✅ 1 click, scrollback intact | ✅ 1 click, ~1.2 s, scrollback intact |
| 5 | Persistence across client exit | ❌ **FAILED.** Probe froze at `17:33:17`, the exact second of Ctrl+Q. Record restored on relaunch; work dead. | ✅ **PASSED.** Probe ran 26 s past `prefix+q` detach, and survived a hard client kill. Re-running `herdr` reattached with **0 interactions**. ⚠️ Dies with the server — §4.6. |
| 6 | Second session on same machine + switch | ✅ 1 click to create (in dialog), 1 click to switch | ✅ 1 click to create, 1 click to switch, ~1.2 s |
| 7 | Tear down a session | ✅ F2 → Delete → Confirm (needs a dock-focus click first). Excellent confirm dialog. Leftovers in §5. | ✅ right-click → Close → Enter. Worktree variant has a best-in-study confirm dialog. Leftovers in §5. |

**Errors encountered**

| Product | Error | Recoverable inside the product? |
|---|---|---|
| `herdr` | `herdr machine add --label testbox testbox` → `usage: …` exit 2 | Yes — the usage line shows the accepted order |
| `fresh` | `Host key verification failed.` on the default remote flow | **No.** "Retry" cannot succeed; the saved-machine dialog exposes no SSH options. Only the unrelated "Other host…" branch offers a workaround, and only to someone who already knows OpenSSH flags. |

---

## 3. Where `herdr` is better

### 3.1 It resolves an unknown host key by handing the user OpenSSH's own prompt, in-band, at the moment of connection — severity 4

`herdr machine add testbox --label testbox` ran `ssh` with a TTY attached, so the participant saw:

```
The authenticity of host '[127.0.0.1]:2222 ([127.0.0.1]:2222)' can't be established.
ED25519 key fingerprint is SHA256:yaB1mVJw9sLw3CbEmT4gp78EPRjbsE/83Ay58RTbIqY.
This key is not known by any other names.
Are you sure you want to continue connecting (yes/no/[fingerprint])? yes
Saved SSH machine ba3e2e926ec49ced79b233743add8f26. Remote server is ready.
```

Typing `yes` completed registration. The key was written to
`/root/sshtest/etc/known_hosts` — the **configured non-default** file — and `~/.ssh/known_hosts`
was never created. Evidence: `herdr-DECISIVE-hostkey-prompt-FULL`, `-accepted-FULL`.

`fresh` at the same moment printed `Host key verification failed.` and offered no way forward.

**For `fresh`:** when the remote step fails host-key verification, do not report failure. Re-run
the connection attached to a TTY (or parse `ssh`'s fingerprint output) and show a modal naming
the host, the key type, and the SHA256 fingerprint, with `[Trust and continue]` / `[Cancel]`.
Accepting must write through OpenSSH's own resolution of `UserKnownHostsFile`, not to a
hard-coded `~/.ssh/known_hosts`.

### 3.2 Creating a session is one click with zero configuration — severity 3

`herdr`'s sidebar `new` button created a workspace in ~0.35 s with no dialog at all. `fresh`
requires opening a modal and clicking `[ Create Workspace ]`, and presents six fields
(Launch in, Machine, Project Path, Workspace Name, Agent, worktree + branch) before the first
session exists. For the overwhelmingly common case — "another shell, here, now" — `herdr` is
one click and `fresh` is a form.

**For `fresh`:** make `[ + New ]` create a workspace immediately using the dock's current
defaults, and move the full dialog behind a secondary affordance (e.g. `[ + New ▾ ]` → "New
workspace with options…", or F2 on the new row). Renaming afterwards is cheaper than configuring
beforehand.

### 3.3 The create button names its target machine — severity 2

Selecting the `testbox` group relabelled the button from `new · Local` to `new · testbox`. The
destination of the next creation is stated on the control that performs it, so it cannot be got
wrong. Evidence: `herdr-13-machine-in-sidebar.txt`.

**For `fresh`:** the dock's `[ + New ]` button is context-free. Label it with the machine the
click will target (`[ + New · testbox ]`), driven by dock selection.

### 3.4 The destructive confirm says what it will *not* do — severity 2

```
delete worktree checkout?
This removes the checkout folder:
/root/.herdr/worktrees/demo/worktree-calm-river-af81
The branch is not deleted. The Herdr workspace will close.
```

Naming the exact path, and explicitly stating that the branch survives, removes the two things a
user actually hesitates over. `fresh`'s dialog (which is otherwise excellent, §4.4) enumerates
what it *will* do but is silent about the branch — and the branch is indeed left behind.

**For `fresh`:** add one line to the delete confirmation: `The branch demo-1 is not deleted.`

### 3.5 A newly registered machine appears in an already-running client — severity 2

`herdr machine add` printed "Open Herdr clients connect automatically", and the running TUI's
sidebar grew a `testbox` group within ~3 s with no restart and no refresh action.

**For `fresh`:** if machine registration ever becomes a thing the user does explicitly, the dock
must pick it up live. (Today `fresh` sidesteps this by auto-discovery — see §4.5.)

### 3.6 Reattaching after a detach costs zero interactions — severity 3

Re-running `herdr` restored the live session, the machine groups, all four workspaces, full
scrollback, and the still-running probe loop. There is no "reattach" command to discover: the
same command you used to start it is the command that brings it back. See §4.6 for the limits.

---

## 4. Where `fresh` is better

`herdr` is the reference, not the winner. `fresh` wins four things outright, one of them
decisively.

### 4.1 Remote machines need no registration step at all — severity 3

On first launch, with no `fresh` state on disk, the Machine dropdown already contained `testbox`,
read from `~/.ssh/config`. Selecting it showed the resolved target `↳ root@127.0.0.1:2222`,
proving it had parsed `HostName`, `Port` and `User` — a genuine trust signal at exactly the
moment the user is deciding whether to commit. Evidence: `fresh-05-machine-dropdown-FULL`,
`fresh-09-testbox-selected-FULL`.

`herdr` has no equivalent. Its TUI has **no path to machine management at all** (§4.7), and even
its CLI requires an explicit `herdr machine add` for a host that is already in `~/.ssh/config`.

**What `herdr` should have done instead:** populate the machine list from `~/.ssh/config` on
startup and treat `herdr machine add` as the way to *pre-warm* a known host, not the way to make
it visible.

Caveat charged to `fresh`: the dropdown's own **"Add machine…" entry is dead** — selecting it
sets the Machine field to the literal string "Add machine…", opens nothing, and silently reverts
to "Local" when you Tab away (evidence `fresh-06-add-machine-FULL`). **Severity 2.** Remove it,
or make it open the "Other host…" fields it appears to promise.

### 4.2 The Orchestrator is discoverable; `herdr`'s machine management is not — severity 3

`fresh` opens with the dock visible, titled **"Orchestrator"**, with `[ + New ]`, a search field
and an overflow affordance. The participant found and used the primary flow in under 20 seconds.

In `herdr`, the participant spent **107 seconds** failing to find machine registration in the UI:
the sidebar `new` button, the `menu` popup (settings / keybinds / reload config / detach), the
tab-bar `+`, right-click context menus on workspaces and on empty sidebar space, `prefix+w`, and
the keybind browser — where filtering for `machine` returned **"no matching keybinds"**. The
feature exists only as `herdr machine <subcommand>` in `--help`. Evidence: `herdr-04`…`herdr-09`.

**What `herdr` should have done instead:** put a "Machines…" entry in the sidebar `menu` popup —
the one place a user already goes looking for global actions.

### 4.3 The active session's machine is always on screen — severity 2

`fresh`'s status bar reads `Trusted  Local` or `Trusted  testbox` and updates on every switch. On
a loopback fixture where hostname and prompt are identical, this was the only on-screen signal
that distinguished local from remote — and it was correct every time. Evidence:
`fresh-22-switch-FULL`, `fresh-16-remote-verify-FULL`.

`herdr` groups workspaces by machine in the sidebar, which answers the same question but only
while the sidebar is visible and only by position in a list where every entry is named identically.

### 4.4 The delete confirmation enumerates its effects — severity 2

```
Delete workspace demo-1 · bash — root@vm: ~/.local/share/fresh/orchestrator/root_work_demo/dem
This will:
  • stop all workspace processes
  • run `git worktree remove`
  • drop the workspace record
Uncommitted changes will be lost.
```

Naming the actual git command it will run is better than any dialog `herdr` showed, and the
uncommitted-changes warning is the right warning. Evidence: `fresh-26-delete-confirm-FULL`.
(It still omits the branch — §3.4.)

### 4.5 Naming and identity of workspaces

`fresh` names local workspaces `demo-1`, `demo-2`, … and shows the terminal title alongside. In
`herdr`, four workspaces in the same repository rendered as four identical `demo / main` rows
with nothing to tell them apart; the participant repeatedly could not tell which was which and
had to fall back on `herdr workspace list` over the socket API to identify them. Renaming exists
in the context menu but nothing prompts for it. **Severity 2 against `herdr`.**

---

## 5. The decisive failure: `fresh`'s remote flow

**Severity 4 (usability catastrophe) — the dock's headline remote capability fails on its
default path, with a misleading error and no in-product recovery.**

### What happened

`[ + New ]` → Machine `testbox` → Project Path `/root/remote-work/demo` → `[ Create Workspace ]`
produced a dock row reading `! ⇅ ssh:testbox  testbox  Host key v…` and a status bar reading
`Orchestrator: Host key verification failed.` (evidence `fresh-10a-HOSTKEY-FAIL`).

F2 on that row offered exactly two actions: **Retry** and **Dismiss** (`fresh-12-f2-menu-FULL`).
Retry failed identically (`fresh-13-retry-FULL`). The dialog for a saved machine exposes no SSH
options field, so there is nothing to adjust (`fresh-14-testbox-fulldialog-FULL`).

### Root cause, proven from observed behaviour

Polling the process table during the failure captured the worktree-preparation command:

```
ssh -o BatchMode=yes -o ConnectTimeout=20 -p 2222 -- root@127.0.0.1 \
    cd '/root/remote-work/demo' || exit 1; REPO=$(git rev-parse --show-toplevel) || exit 1; …
```

It connects to the **resolved address** `root@127.0.0.1 -p 2222`, not to the alias `testbox`.
OpenSSH matches config blocks by the name on the command line, so the `Host testbox` block never
applies and **both** `IdentityFile` and `UserKnownHostsFile` are discarded. Evidence:
`fresh-20-ssh-cmdline-resolved-address.txt`.

The interactive shell step, by contrast, does pass the alias:

```
ssh -t … testbox exec sh -c 'exec ${SHELL:-/bin/sh} -l'
```

Running both forms by hand confirms the consequence exactly:

| Form | Result |
|---|---|
| `ssh -o BatchMode=yes testbox 'echo OK'` | `OK_ALIAS` |
| `ssh -o BatchMode=yes -p 2222 -- root@127.0.0.1 'echo OK'` | `Host key verification failed.` |
| same, with `-o StrictHostKeyChecking=accept-new` | `Permission denied (publickey,…)` |

The third row matters: even if the host-key prompt were fixed, the resolved form would still fail
authentication, because the non-default `IdentityFile` is lost too.

### Reproducible discriminator

With the host key already known to the configured `known_hosts`:

* Machine `testbox`, **Project Path blank** → **succeeds** (alias path only).
* Machine `testbox`, **Project Path set** → **fails** with `Host key verification failed.`

Evidence: `fresh-18-third-attempt-FULL` (two successes) vs `fresh-19-testbox-with-path-FULL`
(failure). So the defect is specifically in the worktree-prep step, and it fires exactly when the
user does the thing the dock is for: open a project on a remote machine.

### `fresh`'s remote support is real — the default path is what is broken

Via the unrelated "Other host…" branch, with Target `testbox` and SSH options
`-o StrictHostKeyChecking=accept-new` (an escape hatch requiring OpenSSH knowledge a first-time
user does not have), the remote workspace worked completely and was verified genuinely remote:

```
SSH_CONNECTION=[127.0.0.1 59022 127.0.0.1 2222]
# REMOTE-DEMO-REPO
/root/.fresh/worktrees/demo/demo-2
```

plus a live `sshd: root@pts/3`, and the host key written to the **non-default**
`/root/sshtest/etc/known_hosts`. Evidence: `fresh-15-otherhost-accept-FULL`,
`fresh-16-remote-verify-FULL`.

So `fresh` does honour `~/.ssh/config` — on the alias path. The fix is narrow and mechanical.

### Secondary findings on the same screen

* The dock truncates the error to `Host key v…`; the full text appears only in the status bar,
  which a user watching the dock will not read. **Severity 2.**
* Remote workspaces created from a saved machine render as `⇅ ssh:testbox  testbox` — the machine
  name twice, the workspace name nowhere — while local ones show `demo-1 · bash — …`.
  **Severity 1.**
* For a remote machine the create dialog shows `[v] Create a git worktree` immediately above
  `↳ Not checked — the host decides when you create`. The glyph says checked, the hint says not
  checked. **Severity 2.** Evidence: `fresh-14-testbox-fulldialog-FULL`.

---

## 6. Persistence: what each product actually guarantees

Tested identically in both: start `while true; do date -u +%H:%M:%S >> /root/probe.txt; sleep 1; done`
inside the session, exit the client, wait ~20 s, check whether the file kept growing.

| | `fresh` | `herdr` |
|---|---|---|
| Client exits (Ctrl+Q / `prefix+q`) | ❌ probe froze at `17:33:17`, the exact quit second, 10 lines total | ✅ probe kept growing to `17:18:06`, 26 s past the `17:17:40` detach |
| Client killed without warning (terminal closed) | not applicable — no process survives Ctrl+Q | ✅ survived; probe reached `17:18:48` after a `17:18:28` kill |
| Session **record** restored on relaunch | ✅ yes — dock repopulates, reconnects to `testbox` | ✅ yes |
| Running **work** restored | ❌ **no** — new shell at `~`, empty scrollback | ✅ yes — same shell, full scrollback, loop still running |
| Interactions to get back | 1 click on the restored row | **0** — `herdr` alone reattaches |
| Survives loss of the background server | n/a (no server) | ❌ **no** — §6.2 |

### 6.1 `herdr`'s remote sessions could not be verified in this fixture — reported as unverified, not as a pass

`herdr`'s architecture is client → server, with a `herdr remote-client-bridge` reached over
`ssh -T`. Because the remote is loopback and shares `$HOME`, the "remote" server resolved to the
**same** `herdr server` process as the local one, over the same socket in `~/.config/herdr`.

Consequences observed:

* The sidebar listed the **same** workspaces `w1`…`w4` under both `Local` and `testbox`; clicking
  a row in the `testbox` group focused `w1`, the local workspace (confirmed via
  `herdr workspace list`).
* A shell opened from the `testbox` group had **`SSH_CONNECTION` empty** and its parent process
  was the local `herdr server` (pid 7169).

The participant attempted to separate them by giving SSH logins a distinct `HERDR_CONFIG_PATH`
via `sshd_config SetEnv`. The bridge inherited the variable but no server ever appeared under the
alternate directory and the sidebar still mirrored. **The participant could not construct a
variant of this fixture in which `herdr`'s remote path resolved to a distinct server.**

Therefore: **`herdr` task 3 is UNVERIFIED.** The UI flow is real and smooth (2 clicks, ~3.2 s) and
the SSH transport is genuinely used (a live `sshd: root@notty` and an `ssh -T … testbox exec
herdr remote-client-bridge` process were present throughout). But the participant did **not**
confirm that the resulting shell ran on the remote host, and does not assert that it did.
`herdr` is a poor comparator for task 3 under loopback, and the row in §2 should be read that way.

`fresh` is unaffected by this: it runs the shell as `ssh -t testbox …`, so `SSH_CONNECTION` is set
and remoteness is directly observable.

### 6.2 `herdr`'s persistence is a property of the server daemon, not of the session

Tested explicitly. After `prefix+q`, `herdr server` re-parented to PID 1 and kept the probe
running. Sending it `SIGTERM` at `17:37:09`:

```
probe after detach:      60 lines (last 17:37:08)
=== KILL SERVER at 17:37:09 ===
probe after server kill: 60 lines (last 17:37:08)
server alive? NO
shell  alive? NO
```

Every workspace shell died with the server — the same frozen-at-the-second signature as `fresh`'s
Ctrl+Q. Relaunching `herdr` started a new server and restored the workspace **record** with a
fresh shell and no running work (evidence `herdr-25-after-server-kill-FULL`).

So the honest framing is not "`herdr` persists and `fresh` does not". It is:

> `herdr` interposes a detachable background server between the UI and the work, so the work
> survives the client and the terminal. `fresh` runs the work as a child of the editor, so it does
> not. **Neither** survives loss of the server process or the machine.

That is the specific, bounded thing `fresh` should adopt — and it is a smaller change than
"add persistence".

---

## 7. What each product left behind after teardown

### `fresh`

| Left behind | Disclosed? |
|---|---|
| `git worktree remove` run, checkout directory emptied | ✅ stated in the dialog |
| Workspace record dropped, processes stopped | ✅ stated in the dialog |
| Branch `demo-1` retained in the user's repo | ❌ not mentioned |
| Stale per-workspace dir under `~/.local/share/fresh/terminals/` | ❌ not mentioned |
| **A worktree `~/.local/share/fresh/orchestrator/.sync-workspace` on a new branch `fresh/fresh-sessions` (commit "Update sessions", containing `sessions.json`), created inside the user's repository** | ❌ **not mentioned anywhere on screen** |
| Stray processes | none — process teardown was clean |

The `.sync-workspace` worktree and `fresh/fresh-sessions` branch were absent from the fixture
before the run and present afterwards. The participant did **not** determine what triggers their
creation and does not claim it is caused by deletion — only that they appeared during the session
and that no dialog, hint or status message ever mentioned that `fresh` writes a branch and a
worktree into the user's repository. **Severity 2** on disclosure grounds alone.

### `herdr`

| Left behind | Disclosed? |
|---|---|
| `git worktree` pruned, checkout folder removed | ✅ path named in the dialog |
| Branch `worktree/calm-river-af81` retained | ✅ explicitly stated |
| Workspace closed, shell reaped (5 → 4 children) | ✅ stated |
| Machine registration in `~/.local/state/herdr/client/endpoints.json` | ❌ `--help` advertises only `~/.config/herdr`; state is split across two trees. **Severity 1.** |
| Stray processes | none |

`herdr`'s teardown disclosure is the better of the two; `fresh`'s is the better *dialog* but hides
more on disk.

### Participant errors, not charged to either product

1. When first testing `herdr`'s context-menu **Close**, the participant captured only screen rows
   1–16 and concluded the action was a no-op. The confirmation dialog was rendering correctly at
   rows 23–28 the whole time. `herdr`'s Close works; the misdiagnosis was the participant's.
2. One click at sidebar row 4 appeared not to switch workspaces. Row 4 was the already-focused
   workspace. Clicking a `herdr` workspace row does switch to it, in one click.

---

## 8. Different but equally valid choices — noted, not scored

* **Create-then-configure vs configure-then-create.** `herdr` creates instantly and lets you
  rename afterwards. `fresh` asks up front and gets the name, path, branch and agent right the
  first time. Both are defensible; they suit different session lifetimes.
* **Where the machine is shown.** `herdr` groups the sidebar by machine (spatial). `fresh` names
  the active machine in the status bar (focal). Each answers the question the other does not.
* **Worktree policy.** `herdr` treats worktrees as an explicit per-action choice ("New worktree"),
  nesting the result under its parent with a tree connector. `fresh` puts a worktree checkbox in
  the create dialog, on by default. Different models of how disposable a session is.
* **Keyboard idiom.** `herdr` uses a `tmux`-style `ctrl+b` prefix; `fresh` uses direct menus, F2
  and a command palette. Both are internally consistent.

---

## 9. Prioritised adoption list for `fresh`

Ordered by experience gap closed per unit of work. Biased, as instructed, toward **changing
defaults and removing misleading affordances** over adding settings and panels.

**P0 — pass the host alias to every `ssh` invocation. (fixes §5, severity 4)**
The worktree-prep step must invoke `ssh … testbox …`, exactly as the shell step already does,
instead of `ssh -p 2222 -- root@127.0.0.1 …`. This single change restores `IdentityFile`,
`UserKnownHostsFile`, `ProxyCommand`, `ProxyJump` and every other directive in the user's `Host`
block. It is the difference between the dock's remote flow working and not working. Keep showing
the resolved `root@127.0.0.1:2222` as a *hint* — it is genuinely reassuring — but never connect
with it.

**P1 — make an unknown host key a question, not a failure. (adopts §3.1, severity 4)**
On host-key verification failure, show a modal with host, key type and SHA256 fingerprint, and
`[Trust and continue]` / `[Cancel]`. Accepting must write through OpenSSH's own
`UserKnownHostsFile` resolution. Until this exists, "Retry" is a control that cannot succeed and
should not be the only one offered.

**P2 — keep workspace processes alive across editor exit. (adopts §3.6/§6.2, severity 3)**
`fresh` already ships a daemon (`fresh --cmd daemon new|attach|list`). Orchestrator workspaces did
not use one: Ctrl+Q killed every session at the exact second. Run orchestrator sessions under a
detached daemon by default so closing the editor detaches instead of terminating, and make
relaunch reattach with zero interactions. Match `herdr`'s bound honestly — survive the client, not
the machine.

**P3 — remove the "Add machine…" entry. (severity 2)**
It opens nothing and silently reverts. `fresh`'s auto-discovery from `~/.ssh/config` is better
than what the entry promises; a dead control beside a working feature only teaches distrust. If a
manual path is wanted, rename it to what actually works: "Other host…".

**P4 — make `[ + New ]` create immediately, with the dialog behind a modifier. (adopts §3.2/§3.3, severity 3)**
One click for the common case; `[ + New ▾ ]` → "New workspace with options…" for the rest. Label
the button with its target machine (`[ + New · testbox ]`) so the destination is stated on the
control that acts.

**P5 — fix the three misleading surfaces on the create/error path. (severity 2)**
(a) `[v] Create a git worktree` above `↳ Not checked` — make the glyph match the hint.
(b) Show the full error in the dock row (wrap or expand on selection) rather than truncating to
`Host key v…` with the real text only in the status bar.
(c) Give remote rows the workspace name: `demo-2 · ssh:testbox`, not `ssh:testbox  testbox`.

**P6 — disclose everything teardown and sync touch. (severity 2)**
Add `The branch demo-1 is not deleted.` to the delete confirmation, clean up the stale
`terminals/` entry, and surface — once, somewhere — that `fresh` maintains a `fresh/fresh-sessions`
branch and a `.sync-workspace` worktree inside the user's repository. Writing a branch into
someone's repo without ever saying so is the kind of thing users discover through `git branch`
and do not forgive.

**P7 — let the dock keep focus. (severity 2)**
Clicking a live workspace moves focus into its terminal, so F2 — the dock's only route to Rename /
Archive / Delete — stops working until you click the dock header to take focus back. Either keep
dock focus on selection, or give the row a visible affordance for its menu.

---

## 10. Severity summary (NN/g 0–4)

| Sev | `fresh` | `herdr` |
|---|---|---|
| **4** | Remote workspace with a project path fails host-key verification; no in-product recovery (§5) | — |
| **3** | Running work does not survive client exit (§6) | No UI path to machine registration; CLI-only (§4.2) |
| **2** | "Add machine…" is a dead menu entry (§4.1) | `machine add` `--help` usage order contradicts the parser |
| **2** | `[v]` vs "Not checked" contradiction (§5) | `?` advertised in the welcome dialog is swallowed by the shell unless prefixed |
| **2** | Error truncated in dock, full text only in status bar (§5) | Every workspace in a repo is labelled identically (§4.5) |
| **2** | Undisclosed `fresh/fresh-sessions` branch + `.sync-workspace` worktree (§7) | — |
| **2** | Dock loses focus on selection, disabling F2 (§9 P7) | — |
| **1** | Remote rows show the machine twice and the name never (§5) | Sidebar header renders `" spaces"` instead of `"workspaces"` before any machine is registered |
| **1** | Branch retained on delete, not mentioned (§3.4) | State split across `~/.config/herdr` and `~/.local/state/herdr`; `--help` names only the first (§7) |

---

## 11. Honest reporting

### Completed

**`herdr`:** tasks 1, 2, 4, 5, 6, 7 completed and verified. Task 3's *flow* completed; its
*result* was not verified (below).

**`fresh`:** tasks 1, 2, 4, 5, 6, 7 completed and verified. Task 3 **failed** on the default path
and is reported as a failure.

### Could not complete, and why

* **`fresh`, task 3 (session on remote machine) — FAILED.** `Host key verification failed.`, no
  in-product recovery, cause proven in §5. It succeeded only via an escape hatch ("Other host…"
  plus a hand-typed `-o StrictHostKeyChecking=accept-new`) that a first-time user would not find,
  and, separately, with a blank Project Path — which is not the task.
* **`herdr`, task 1 in the UI — FAILED.** Machine registration is not reachable from the running
  TUI. It succeeded via the CLI after ~107 s of unsuccessful UI search.

### Could not verify

* **`herdr`'s remote sessions are genuinely remote.** Under loopback with a shared `$HOME`, the
  remote `herdr` server is the local one. `SSH_CONNECTION` was empty inside `testbox` workspaces
  and the sidebar mirrored the same `w1`…`w4` under both machines. An attempt to separate the two
  servers did not succeed. Reported as unverified; no claim is made either way. §6.1.
* **`herdr`'s remote persistence.** Follows from the above — the local-session persistence result
  (PASS) is solid, but it was not demonstrated for a session on a genuinely separate host.
* **What creates `fresh`'s `.sync-workspace` worktree and `fresh/fresh-sessions` branch.** Their
  appearance during the session is recorded; the trigger was not determined and is not guessed at.
* **Why one `fresh` attempt on the saved machine failed while the host key was already known.**
  It was later shown to be reproducible and tied to setting a Project Path (§5), but the
  intermediate attempt was not separately instrumented.

### Measurement caveats

* Wall-clock times include fixed harness sleeps between synthetic input events and **overstate**
  both products. Action counts are the trustworthy figures. Two product-latency numbers were
  measured cleanly and are stated as such: `herdr machine add` at 1.1 s with a known host key, and
  `herdr`'s 1-click workspace creation at ~0.35 s.
* `herdr` was tested first. The participant was therefore marginally more practised at driving a
  TUI through `tmux` by the time `fresh` was tested. Both participant aiming errors (§7) occurred
  during the `herdr` run and are excluded from `herdr`'s counts.
* Evidence captures for `herdr` tasks 1–7 are plain text and several show only part of the screen;
  the switch to full-screen colour capture happened mid-study. The decisive `herdr` moment was
  re-captured in full colour afterwards. All `fresh` captures are full-screen with colour.
* One `~/.ssh/known_hosts` entry was created by the participant's own command-line diagnostic in
  §5 (row 3 of the table), after both products' flows had been exercised. It was deleted
  immediately and did not affect any product measurement.

---

*Raw evidence: `docs/internal/comparative-study-evidence-cold/` — see the README there for an
index of the decisive screens.*
