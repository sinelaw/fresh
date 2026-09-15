# Comparative Benchmark Usability Study: Machine & Session Creation

**fresh Orchestrator vs. herdr (baseline)**

**Method:** Comparative benchmark usability test (Nielsen Norman Group discount protocol),
single participant, think-aloud, cold start, identical task script and fixture environment
run against both products.
**Date:** 2026-09-15
**Scope:** machine-creation and session-creation flows only.

---

> ## ⚠️ CORRECTION — the herdr half of this study is VOID (wrong version)
>
> **This study tested `herdr 0.1.0`, which is not current herdr.** It was installed from
> crates.io, where `0.1.0` is the only version ever published and the channel has been
> abandoned. The project's real release channel
> (`curl -fsSL https://herdr.dev/install.sh | sh`) was at **v0.9.0** on the date of this
> study — **28 tagged releases later**.
>
> **Every conclusion below about herdr is therefore about an obsolete build and must not be
> read as a statement about herdr.** In particular, §1.3, §5.1 and §10 claim herdr "has no
> SSH or remote capability" and "is not a valid baseline". That is true of `0.1.0` and
> **false of herdr 0.9.0**, whose CLI includes `herdr --remote <ssh-target>`,
> `herdr machine <subcommand>` for managing saved SSH machines, `herdr session attach`, and a
> persistent background server. Its 0.9.0 release notes describe exactly the flow this study
> set out to benchmark: *"Manage Local and saved SSH machines from one Herdr window ... Add
> and manage connections with `herdr machine`; a disconnected machine does not interrupt the
> others."* The original brief's `herdr --remote` claim was correct; this study's rejection
> of it was an artifact of the wrong version.
>
> **The `fresh` half of this study stands and is unaffected.** Those findings were produced
> by direct observation of fresh and verified independently of herdr — including the two that
> matter most: that prior findings F1 and F2 do **not** reproduce (fresh delegates to the
> system `ssh` with the host alias, so the whole `~/.ssh/config` is honored), and that fresh
> instead fails **open**, silently recording an unverified host key under
> `StrictHostKeyChecking=accept-new`. The prioritized adoption list in §9 is likewise derived
> from observed fresh behaviour and remains valid, except where it cites herdr as the
> comparator for persistence (§4.4, §7.2) — herdr 0.9's server model must be re-measured
> before those comparisons mean anything.
>
> **Superseded by:** `workspace-sessions-comparative-study-herdr-0.9.md`, which re-runs the
> same seven-task script against herdr 0.9.0.
>
> *Root cause, for the record: the brief pinned `herdr = "0.1.0"` and `cargo install`
> succeeded, so the fallback to a release binary was never triggered and the crates.io index
> was treated as authoritative for "latest". A project's own release channel should have been
> checked before declaring a baseline unavailable.*

---

---

## 1. Headline results

Three findings dominate this study, and two of them contradict the premises the study
was commissioned under. They are stated up front because everything else is detail.

1. **The two Severity-4 findings of the prior study (F1, F2) do not reproduce on this
   build.** fresh created the remote `testbox` session **successfully, in 4 clicks, with
   zero errors and zero out-of-band shell work**, under the deliberately hostile fixture
   (non-default `IdentityFile`, non-default `UserKnownHostsFile`, host never previously
   connected to). fresh does this by **delegating to the system `ssh` binary with the host
   alias**, which makes the user's entire `~/.ssh/config` authoritative by construction —
   which is precisely what the prior study recommended as the fix for F2.

2. **fresh does not fail closed on an unknown host key. It fails _open_.** It passes
   `StrictHostKeyChecking=accept-new` and silently records the unverified key. No
   fingerprint, no prompt, no notification. The prior study's F1 recommendation
   (show fingerprint, ask to trust) is still the right change, but for the opposite
   reason: the risk is not a dead end, it is an **unannounced trust decision made on the
   user's behalf**. This is the single most important adoption item in this report.

3. **herdr 0.1.0 cannot perform the remote half of the task script at all, and its
   persistence is not what it is advertised to be.** It has no SSH or remote capability
   of any kind, and its "background persistence" is a saved `session.json` of names,
   layout and working directories — not surviving processes. **Neither product keeps work
   running when the client exits.**

The brief's premise that `herdr --remote` "reads your SSH host config and attaches without
a manual SSH login, which is exactly the flow fresh fails at today" is **false in both
halves**: that flag does not exist, and fresh does not fail at that flow.

---

## 2. Method

### 2.1 Participant persona

A deliberately constructed first-time user: no demo, no video, no source reading, no user
documentation during the task phase. The tester operated only on what each product reveals
on screen. Both products were driven inside `tmux` (200x50, mouse reporting on), input sent
with `tmux send-keys`, screens read with `tmux capture-pane`.

**Declared bias — read this before trusting the action counts.** The tester was required to
read the prior fresh study first, and therefore was *not* cold on fresh's UI, while being
genuinely cold on herdr. This asymmetry pushes **fresh's action counts downward** and its
times down with them. Action counts below are therefore a *lower bound* for fresh and a
*realistic* figure for herdr. Where a known fresh trap exists, the tester deliberately
walked into it to check whether it still reproduces (see §6). Wall-clock times include
fixed scripted waits in the harness and should be read as ordinal, not absolute; **action
counts are the reliable metric.**

### 2.2 Products under test

| | fresh | herdr |
| --- | --- | --- |
| Version | 0.5.1 | 0.1.0 |
| Build | `cargo build` (debug), `target/debug/fresh` | `cargo install herdr --version 0.1.0` (release) |
| Commit / provenance | code at `da558dd`; branch head `2fd7bcd` adds only the prior study doc | crates.io, published 2026-03-27T18:20:41Z |
| Shape | editor with an Orchestrator dock panel | terminal workspace manager / multiplexer |
| Stack (observed) | — | ratatui 0.30, portable-pty 0.9, vt100 0.16, tui-term 0.3, crossterm 0.29, tokio 1 |

`herdr` installed cleanly from the registry on the first attempt. GitHub
(`github.com/herdrdev/herdr`) is blocked by this session's egress proxy, but no source
access was needed or wanted — the crate and its own on-screen help were sufficient, and
reading source would have violated the protocol.

### 2.3 Fixture environment

Identical for both products, and **reset to byte-identical state between the two runs**
(fixtures rebuilt, product state directories deleted, and critically both `known_hosts`
files removed again after fresh wrote one).

- **SSH server:** user-space `sshd`, `127.0.0.1:2222`, ed25519 host key, pubkey auth only,
  `PasswordAuthentication no`.
- **SSH client config:** `~/.ssh/config` alias `testbox` → `HostName 127.0.0.1`, `Port 2222`,
  `User root`, **`IdentityFile /root/sshtest/etc/testkey`** (non-default),
  **`UserKnownHostsFile /root/sshtest/etc/known_hosts`** (non-default). No
  `StrictHostKeyChecking` directive.
- **The discriminating condition:** neither `~/.ssh/known_hosts` nor the configured
  `known_hosts` existed at the start of either run, and the key was never copied to a
  default path. Because password auth is disabled and the key sits at a non-default path,
  **any successful connection proves both `IdentityFile` and the host-key path were
  honored.** This was verified immediately before each run.
- **Git fixtures:** `/root/fixtures/acme-widgets` and `/root/fixtures/acme-widgets-remote`
  (distinguishable README), 3 commits, branches `main`, `feature/gearbox`, `release/0.1`.

### 2.4 Scope difference, declared

fresh is an **editor with an orchestrator dock**; herdr is an **agent multiplexer**. Neither
is penalised below for lacking something outside its remit, and §8 lists the places where
they simply made different, equally defensible choices. The one place this matters most:
herdr scoring "not supported" on Tasks 1 and 3 is a **statement of remit, not a defect** —
but it does mean herdr cannot serve as the baseline for those tasks, which is the single
biggest limitation of this study (§10).

---

## 3. Task-by-task comparison

Actions are counted as discrete user acts (a click, or a keystroke). Typed names are counted
as their character count, shown separately. "Recovery without leaving the product" asks
whether a user who hit an error could resolve it without dropping to a shell.

| # | Task | fresh | herdr |
| --- | --- | --- | --- |
| 1 | **Register machine `testbox`** | ✅ **0 actions.** Already present in the Machine list at first open, resolved to `root@127.0.0.1:2222`. 0 errors. | ⛔ **Not supported.** No machine concept exists anywhere in the product. |
| 2 | **Session on local machine** | ✅ **2 clicks**, 0 typing, ~6 s. 0 errors. | ✅ **2 keystrokes + 10-char name**, ~4 s. 0 errors. |
| 3 | **Session on remote `testbox`** | ✅ **4 clicks**, 0 typing, ~4 s. **0 errors.** | ⛔ **Not supported as a product feature.** Reachable only by typing `ssh testbox` into a pane (16 keystrokes) — that is OpenSSH's work, not herdr's, and the result is an unmanaged process. |
| 4 | **Return to a session** | ✅ **1 click**, ~1 s. Scrollback intact. | ✅ **2 keystrokes** (prefix + digit), ~0.5 s. Scrollback intact. |
| 5 | **Persistence across client quit** | ⚠️ **Partial.** Quit unwarned; running job killed. Records survive; **1 click** reconnects and re-establishes SSH. List **reordered**, focus lost, scrollback lost. | ⚠️ **Partial.** Quit unwarned; running job killed. **0 actions** — workspaces, order, focus and cwd restored automatically. Scrollback lost. An SSH pane comes back as a **local** shell. |
| 6 | **Second session on same machine + switch** | ✅ Create **4 clicks** (machine selection is sticky, so 2 suffice); switch **1 click**. ⚠️ **Both remote rows render identically** — indistinguishable. | ✅ Create **2 keystrokes + 11-char name**; switch **2 keystrokes**. Rows distinctly named and numbered. |
| 7 | **Tear down a session** | ✅ **3 actions.** Exemplary confirm dialog. ⚠️ Leaves an undisclosed branch **and writes two undisclosed objects into the user's repo**. | ✅ **3 keystrokes.** Terse confirm with no consequences and no key hint. Leaves the repo **perfectly clean**. |

**Errors encountered, whole study:** fresh **0**; herdr **0** (within remit). Neither product
required the tester to leave it to recover from an error, because neither produced one.
This is a marked improvement on the prior study, which recorded 2 blocking errors and 2
out-of-band shell fixes for fresh on Task 3 alone.

---

## 4. Where herdr is better, and precisely why

These are the adoption recommendations. Each names an interaction and a moment.

### 4.1 A persistent keymap legend, always on screen — *Adopt*

**The interaction:** herdr reserves the bottom two lines for a live legend:
`n new  N rename  d close  v split  - split  ⇥ pane  f full  r resize  b sidebar  q quit`,
with a mode indicator (`NAVIGATE`, `NEW`, `CLOSE?`) in the sidebar's top-left corner.
**The moment:** every moment. The tester never had to guess or probe for a command.

Against this, fresh's recovery actions (`Retry`, `Dismiss`) and per-session actions
(`Visit…`, `Rename…`, `Archive`, `Delete`) live **only behind a right-click**, with nothing
on screen indicating right-click exists — the prior study's F4, which reproduced exactly.
fresh also has no legend for its status glyphs (`·`, `*`, `!`, `⇅`).

> **Adopt:** a one-line legend in the dock footer, and a `⋯` affordance on each row that
> opens the same menu as right-click. This removes a hidden affordance rather than adding
> a panel.

### 4.2 An explicit, always-visible prefix key that resolves "who gets my keystroke" — *Adopt*

**The interaction:** herdr's header permanently reads `ctrl+s ⏎`. Keys go to the terminal;
`ctrl+s` enters app mode.
**The moment:** the tester pressed `?` expecting product help and got *bash completion*
("Display all 1569 possibilities?"). The header told them immediately why, and `ctrl+s`
fixed it. The ambiguity existed for one keystroke and was self-correcting.

fresh has the same ambiguity — a focused workspace terminal swallows keys — with **no
indicator at all** (prior F11). Worse, fresh's status bar permanently advertises
`Palette: Ctrl+P`, and **Ctrl+P does nothing**, dock-focused or editor-focused. That is an
affordance that actively lies.

> **Adopt:** either make `Ctrl+P` work or stop advertising it — a promise in the status bar
> is worse than silence. Then show which surface has focus.

### 4.3 The workspace list is addressable and every row is distinguishable — *Adopt, highest UX value*

**The interaction:** herdr renders `1 · acme-local` / `2▸· acme-second`. The number is the
jump key: `ctrl+s 2`.
**The moment:** after creating a second session, the tester could name the one they wanted
and reach it in two keystrokes.

fresh's failure here is severe and is the sharpest regression this study found. After
creating a **second** session on `testbox`, the dock read:

```
  · ⇅ ssh:testbox  testbox
  · ⇅ ssh:testbox  testbox
```

Two byte-identical rows. The workspace names (`acme-widgets-1`, `acme-widgets-2`) that
fresh itself generated and stored are **absent from remote rows entirely**, and both editor
tabs also read `ssh — root@vm: ~`. The only difference is the `*`/`·` active marker. The
sessions *were* genuinely distinct (`/dev/pts/2` vs `/dev/pts/4`), but nothing on screen
says which is which. With three or more remote sessions this is unnavigable.

> **Adopt:** lead every row with the workspace name; demote host and command to a dimmed
> suffix. This is a pure label-ordering change and closes the biggest day-two gap in the
> feature.

### 4.4 Restore puts you back exactly where you were, with no clicks — *Adopt*

**The interaction:** relaunching herdr restores every workspace with its name, list order,
**which one was focused**, and its cwd, with **zero user actions**.
**The moment:** the tester typed `herdr` and was looking at the same workspace they had quit
from.

fresh restores the session *records*, which is good, but the tester was dropped on the
Welcome tab; the list had **silently reordered** (the SSH row jumped above the project row,
prior F18); and it took a click to get back. Dock width was also not persisted.

> **Adopt:** persist list order and last-focused session, and reopen it on launch.

### 4.5 Sessions leave the user's repository untouched — *Adopt the disclosure, not the behaviour*

**The interaction:** herdr never touches git. After creating, using and closing two
workspaces, `git worktree list`, `git branch` and `git status --porcelain` in the fixture
repo were **byte-identical to before the run**.

fresh's teardown, by contrast:

```
# before delete
/root/fixtures/acme-widgets                            [main]
/root/.local/share/fresh/.../acme-widgets-1            [acme-widgets-1]
branches: feature/gearbox  main  release/0.1  acme-widgets-1

# after deleting workspace acme-widgets-1
/root/fixtures/acme-widgets                            [main]
/root/.local/share/fresh/orchestrator/.sync-workspace  [fresh/fresh-sessions]   <-- NEW
branches: feature/gearbox  main  release/0.1  acme-widgets-1  fresh/fresh-sessions
```

Three things survive that the confirm dialog does not mention: the branch
`acme-widgets-1`, a `.sync-workspace` worktree, and a `fresh/fresh-sessions` branch. The
latter two were **absent before the delete and present after it** — the cleanup path
silently created new objects in the user's repository. A user who inspects their own repo
finds refs they never made, and may reasonably delete them or report corruption.

fresh's worktree-per-session model is a **legitimate and arguably better** design than
herdr's (§8), so the recommendation is not to stop — it is to **stop doing it silently**.

> **Adopt:** add the surviving branch to the delete dialog's consequence list (ideally with
> an "also delete branch" checkbox), and disclose the `fresh/fresh-sessions` bookkeeping
> once, on first use.

### 4.6 The product is fully usable from the keyboard *and* the mouse — *Adopt*

herdr is keyboard-native, and clicking a sidebar row also switches workspace — verified.
It supports both.

fresh's Orchestrator dock is **mouse-only**. `View ▸ Orchestrator Dock` carries **no
accelerator**, conspicuously next to `File Explorer  Ctrl+B` in the very same menu; there is
no "New Workspace Session" command anywhere; and the advertised palette does not work. In a
terminal-first editor this is the most surprising gap in the feature.

> **Adopt:** give the dock an accelerator and add session commands to the menus.

---

## 5. Where fresh is better, and what herdr should have done

herdr is the reference, not the winner. On the study's actual subject — **machine and
session creation** — fresh is substantially the stronger product.

### 5.1 fresh has a machine model at all; herdr has none

This is the decisive difference. fresh's Machine control offers `Local`, the discovered
`testbox`, `Other host…`, `Kubernetes…` and `Devcontainer`. herdr offers nothing: the
`NAVIGATE` keymap has no host key, `--help` lists only `--no-session`, `--default-config`,
`--version`, `--help` and an `update` command, `--default-config` contains no host key, and
the binary contains **zero occurrences of the substring `ssh`**.

> **herdr should have:** shipped any first-class notion of a remote target. Until it does,
> "remote SSH sessions" is not a feature it has.

### 5.2 Zero-action machine registration, from config the user already wrote

fresh's Task 1 cost **no actions at all**: `testbox` was in the dropdown on first open,
correctly resolved to `root@127.0.0.1:2222`. This remains, as the prior study said, the most
delightful moment in the product — and this study can now add that **the promise is kept**,
which the prior study concluded it was not.

### 5.3 Remote connection honours the user's entire SSH config, by construction

fresh spawns the system `ssh` with the **alias**, not a reconstructed host/port/user tuple:

```
ssh -o StrictHostKeyChecking=accept-new -o BatchMode=yes testbox python3 -u -c "..."
ssh -t -o StrictHostKeyChecking=accept-new testbox exec sh -c 'exec ${SHELL:-/bin/sh} -l'
```

Because `testbox` is passed through, `IdentityFile`, `UserKnownHostsFile`, `ProxyJump` and
everything else in the user's config apply automatically. The proof is in the fixture:
auth succeeded with password auth disabled and the key at a non-default path, and the host
key was written to the **configured** `known_hosts`, while the default one stayed absent.

This is the **correct architecture** and it is worth stating plainly, because the prior
study proposed it as a fix for a defect that this build does not have.

### 5.4 The destructive-action dialogs are far better than herdr's

fresh, on delete:

```
Delete workspace acme-widgets-1?
This will:
  • stop all workspace processes
  • run `git worktree remove`
  • drop the workspace record
Uncommitted changes will be lost.
[ Cancel ]  [ Confirm Delete ]
```

and, on archive, it also states **"Reversible via Unarchive."** Consequences enumerated,
data loss called out, `Cancel` in the first safe position.

herdr's entire close confirmation is the word **`CLOSE?`** in the sidebar corner. It does not
say what will be lost, does not say the pane's processes will die, and — the real defect —
**does not say which key confirms**. The tester guessed `y`. The footer legend, present
everywhere else, is blank in exactly the one mode where a wrong guess is destructive.

> **herdr should have:** kept the footer legend populated in `CLOSE?` mode with `y confirm /
> esc cancel`, and named the workspace being closed.

### 5.5 Git worktree isolation, with well-chosen defaults

fresh creates a real worktree on a new branch so two sessions on one repo cannot collide.
herdr opens every workspace in the same shared cwd — two herdr workspaces on one repo are
two shells in the same working tree, free to trample each other. For the *agent* workloads
both products target, fresh's isolation is the more valuable default.

fresh's defaults are also better: project path prefilled from cwd, workspace name
auto-generated, machine selection remembered between dialogs (the second remote session
needed only 2 clicks, not 4). herdr requires a name every time and offers no default.

### 5.6 Context-sensitive hints

fresh's hint line re-writes itself per machine — `this computer` → `root@127.0.0.1:2222`,
and `blank uses this default` → `/srv/project (blank = remote home)` — and reflects real repo
state (`no origin configured`). herdr has no equivalent. (fresh's hints are not uniformly
correct; see §6.)

---

## 6. Prior-study findings re-checked on this build

The tester deliberately re-walked the known traps.

| Prior finding | Status in this run |
| --- | --- |
| **F1** Host key verification failed, no TOFU — Sev 4 | **Does not reproduce.** Connection succeeded. **But the underlying concern is worse, not gone** — see §7.1. |
| **F2** `~/.ssh/config` partially honored — Sev 4 | **Does not reproduce.** Full config honored via system `ssh`; proven by non-default `IdentityFile` + `UserKnownHostsFile` both working. |
| **F5** worktree checkbox glyph/caption/behaviour disagree | **Reproduces.** `[v] Create a git worktree` above `↳ Not checked — the host decides when you create`. |
| **F8** undisclosed writes to the user's repo | **Reproduces**, and is worse than described: the objects appear during **teardown**, not creation. |
| **F9** deleted workspace leaves its branch | **Reproduces.** |
| **F10** quitting with a live remote session gives no warning | **Reproduces.** Ctrl+Q killed a live SSH session instantly. (herdr's `q` does the same.) |
| **F11** advertised `Ctrl+P` palette does nothing | **Reproduces.** |
| **F12** no keyboard route to the dock | **Reproduces.** `File Explorer Ctrl+B` vs `Orchestrator Dock` (no accelerator). |
| **F13** `▼` disclosure arrow is inert | **Does not reproduce.** The arrow opened the list reliably. |
| **F15** session labels inconsistent/over-long | **Reproduces, and understates it** — see §4.3. |
| **F18** list reorders after restart; dropdown text bleed-through | **Both reproduce.** Popup bled `:2222` through its right edge. |

---

## 7. The one thing neither product gets right

### 7.1 fresh makes a security decision for the user, silently

This deserves its own section because it is the most consequential finding and it **inverts**
the prior study's read of the same area.

At the moment of first contact with an unknown host, the two products behave like this.

**herdr** (i.e. plain OpenSSH in a pane) — `evidence: herdr-17-ssh-tofu-prompt.txt`:

```
The authenticity of host '[127.0.0.1]:2222 ([127.0.0.1]:2222)' can't be established.
ED25519 key fingerprint is SHA256:neVTlyuLdLK4/Xl1ZGhoH9TqQK1wqH5Fc7OiLY23hr4.
This key is not known by any other names.
Are you sure you want to continue connecting (yes/no/[fingerprint])?
```

**fresh** — `evidence: fresh-04-remote-attempt1.txt`: a working shell. Nothing else. No
fingerprint, no prompt, no notice. The key was written to `known_hosts` and the user was
never told a trust decision had been made.

fresh is *faster*, and for the benchmark it *wins the task*. But `StrictHostKeyChecking=accept-new`
means fresh silently accepts whatever key answers on first contact — weaker than what the
user's own `ssh` would have done with the exact same config, and weaker than the user has
any reason to expect. The product quietly overrides the user's security posture: the passed
flag beats the user's config file.

> **The fix is the prior study's F1 recommendation, with a corrected rationale:** on an
> unknown host key, show host, port and fingerprint and offer *Trust and connect* / *Cancel*,
> writing to `known_hosts` on accept. Do not remove the delegation to system `ssh` — that
> part is right. Remove the `accept-new` override and surface the decision.

### 7.2 Neither product has background persistence

Both were tested identically: a `while true; do date >> file; sleep 1; done` loop left
running, then the client quit, then relaunched.

| | fresh | herdr |
| --- | --- | --- |
| Client quit warned? | No | No |
| Probe kept ticking? | **No** — froze at the quit second | **No** — froze at the quit second |
| Surviving PTYs / sshd sessions | 0 | 0 |
| Restored on relaunch | records, reordered, 1 click to reconnect | names + order + focus + cwd, 0 clicks |
| Scrollback | lost | lost |

herdr's `session.json` stores `name`, `layout`, `cwd` and `focused` — and nothing else. Its
log shows `session saved workspaces=2` then `herdr exiting`, with the children dying with it.

So herdr's advertised "holds real terminals open so the work survives the lid closing" is,
in 0.1.0, **layout restore**. It is a better restore than fresh's (§4.4), but it is the same
*class* of feature, and **herdr is not the reference implementation of persistence this study
was sent to find.**

fresh is meaningfully ahead in one respect: it records the *machine*, so one click
re-establishes the SSH connection. herdr, having no remote concept, restores an SSH pane as
a **local shell at the local cwd** — silently putting the user on a different machine than
the one they left. Verified: `herdr-19-ssh-not-restored.txt`, zero sshd sessions after
restore.

---

## 8. Different but equally valid choices

Noted, not scored.

- **Where a session lands.** fresh: an isolated worktree under
  `~/.local/share/fresh/orchestrator/<slug>/<name>`. herdr: the project directory itself.
  Isolation vs. immediacy — both defensible. (fresh should still *preview the path* before
  creating; landing in an unexplained deep directory surprised the prior tester too.)
- **Naming.** fresh auto-generates (`acme-widgets-1`), herdr requires the user to type one.
  Fewer actions vs. more meaningful labels.
- **Primary input.** herdr is keyboard-first with a prefix key; fresh is mouse-first with
  dialogs. Both are reasonable defaults — but fresh's dock being mouse-*only* is not (§4.6).
- **Multiplexing.** herdr has panes, splits, zoom and resize inside a workspace; fresh has
  editor tabs and a dock. Different products; out of this study's remit.
- **Confirmation weight.** fresh's enumerated dialog vs. herdr's single-keypress confirm.
  Both are coherent within their own interaction model, even though herdr's specific
  execution is under-labelled (§5.4).

---

## 9. Prioritized adoption list for fresh

Ordered by experience gained per unit of work. Biased, as requested, toward **changing
defaults and removing misleading affordances** over adding settings and panels.

| # | Change | Type | Why it is first |
| --- | --- | --- | --- |
| **1** | **Show the host fingerprint and ask before trusting.** Drop `StrictHostKeyChecking=accept-new`; on an unknown key show host/port/fingerprint with *Trust and connect* / *Cancel*. | Default change | The only finding in this study with a security consequence. fresh currently overrides the user's own SSH posture silently (§7.1). |
| **2** | **Put the workspace name first on every session row and tab.** Demote host/command to a dimmed suffix. | Label change | Today two remote sessions are byte-identical on screen and cannot be told apart (§4.3). Pure win, no new UI. |
| **3** | **Stop advertising `Ctrl+P` unless it works**, and give the Orchestrator dock an accelerator alongside `File Explorer`'s `Ctrl+B`. | Remove false affordance + default | A status bar that promises a shortcut that does nothing is worse than silence; the dock is currently mouse-only (§4.2, §4.6). |
| **4** | **Surface row actions inline.** Render `[Retry]` / `[Dismiss]` on error rows and a `⋯` button on every row mirroring the right-click menu. | Remove hidden affordance | Recovery is currently reachable only by a right-click nothing advertises (§4.1). |
| **5** | **Tell the truth in the delete dialog.** Add the surviving branch to the consequence list (with an optional "also delete branch"), and disclose `fresh/fresh-sessions` / `.sync-workspace` once on first use. | Disclosure | The dialog is excellent precisely *because* it enumerates consequences; two are missing, and two objects appear in the user's repo unannounced (§4.5). |
| **6** | **Restore to where the user left.** Persist list order, last-focused session and dock width; reopen that session on launch. | Default change | herdr costs 0 actions here; fresh costs a click and a moment of "where did it go?" (§4.4). |
| **7** | **Warn before quitting with live sessions**, matching the care already taken on delete. | Default change | Today the *most* destructive path is the *only* unguarded one (§7.2, prior F10). |
| **8** | **Fix the worktree checkbox's three-way contradiction** and add a footer legend for glyphs and keys. | Correctness + legend | Glyph says checked, caption says "Not checked", behaviour creates a worktree (prior F5). |

Items 1–4 are the smallest set that closes the largest gaps; 1 and 2 alone address the two
sharpest problems this study found.

---

## 10. Honest limitations

- **herdr is not a valid baseline for Tasks 1, 3, or the remote half of 5.** It has no remote
  capability, so for the study's headline question — how fresh's *machine* creation compares
  to a product that does it well — **this study found no comparator.** Where §4 credits
  herdr on remote behaviour (§7.1), the credit belongs to **OpenSSH**, which herdr merely
  hosts in a pane; that is stated wherever it applies. A genuine baseline for fresh's remote
  flow would need a different product (a Mosh/Eternal-Terminal-class tool, VS Code Remote-SSH,
  or tmux/Zellij with a detach-and-reattach story).
- **The tester was not cold on fresh** (required prior reading), while genuinely cold on
  herdr. fresh's action counts are a lower bound; herdr's are realistic. This biases the
  quantitative comparison **in fresh's favour** and should be assumed wherever the two are
  close (Tasks 2, 4, 6).
- **Wall-clock times include fixed harness sleeps** and are ordinal only. Action counts are
  the trustworthy metric.
- **Single participant, single session, one platform** (Ubuntu 24.04, tmux 3.4, 200x50).
  Discount-protocol findings; severity judgements are the tester's.
- **"Remote" was loopback SSH to the same host.** Auth, host-key and config-resolution
  behaviour are fully exercised and the connections were proven genuine via `SSH_CONNECTION`
  and live `sshd` sessions, but latency, disconnection and reconnection-under-packet-loss
  were **not** tested — and those are exactly where a persistence feature earns its keep.
- **Could not verify:** why the prior study saw F1/F2 and this run did not. Same code, same
  binary path, different fixture details (the prior config carried `StrictHostKeyChecking no`
  and a `~`-relative `IdentityFile`). The discrepancy is real and unexplained; this report
  states only what was observed here, with evidence.
- **Not tested, by protocol:** `Kubernetes…` and `Devcontainer` machine types, `Other host…`
  manual entry, fresh's agent types beyond `terminal`, and herdr's pane splitting — all
  outside the machine/session-creation flows in scope.

---

## 11. Raw evidence

All captures are verbatim `tmux capture-pane` output taken at the moment described, in
`docs/internal/comparative-study-evidence/`.

| File | What it shows |
| --- | --- |
| `00-environment.txt` | Versions, commits, provenance, herdr's CLI surface, the zero-`ssh`-strings result |
| `fresh-01-machine-list.txt` | `testbox` already in the Machine list — Task 1 at zero cost |
| `fresh-02-testbox-selected.txt` | Resolved hint `root@127.0.0.1:2222`; the F5 checkbox contradiction |
| `fresh-03-local-session.txt` | Task 2 complete in 2 clicks |
| `fresh-04-remote-attempt1.txt` | **Task 3 succeeding with no host-key prompt** — the §7.1 moment |
| `fresh-05-remote-proof.txt` | `SSH_CONNECTION=[127.0.0.1 41992 127.0.0.1 2222]` — proof it is genuinely remote |
| `fresh-06-ssh-invocation.txt` | The live `ps` lines proving delegation to system `ssh` with the alias |
| `fresh-07…09` | Return to session; restart; 1-click reconnect |
| `fresh-11-switch-ambiguous.txt` | **Two byte-identical remote rows** — the §4.3 moment |
| `fresh-14-archive-confirm.txt`, `fresh-15-delete-confirm.txt` | The exemplary dialogs (§5.4) |
| `fresh-12`/`16`/`18-*teardown*.txt` | Repo state before and after delete; the undisclosed leftovers |
| `fresh-17-view-menu.txt` | `File Explorer Ctrl+B` beside an accelerator-less `Orchestrator Dock` |
| `herdr-01-new-dialog.txt`, `herdr-04-prefix.txt` | The empty state, the `Name:` prompt, the persistent legend and prefix |
| `herdr-05-cli-help.txt`, `herdr-06-default-config.txt` | **No `--remote`, no host/ssh config keys** |
| `herdr-07…09` | Two named, numbered workspaces; prefix+digit switching; scrollback intact |
| `herdr-10-session-json.txt`, `herdr-11-herdr-log.txt` | What persistence actually stores: names, layout, cwd — no processes |
| `herdr-12-after-restart.txt` | Zero-action restore preserving order and focus |
| `herdr-14-close-confirm.txt` | The bare `CLOSE?` with no consequences and no key hint |
| `herdr-16-post-teardown.txt` | Repo perfectly clean after teardown |
| `herdr-17-ssh-tofu-prompt.txt` | **OpenSSH's fingerprint + yes/no prompt** — the §7.1 contrast |
| `herdr-19-ssh-not-restored.txt` | An SSH pane restored as a local shell |
| `herdr-20-mouse-test.txt` | herdr's sidebar responds to the mouse as well as the keyboard |

### Reproduction

```bash
# sshd fixture (non-default IdentityFile AND UserKnownHostsFile; never connected before)
mkdir -p /root/sshtest/etc && cd /root/sshtest
ssh-keygen -q -t ed25519 -f etc/ssh_host_ed25519_key -N ''
ssh-keygen -q -t ed25519 -f etc/testkey -N '' -C testuser
cat etc/testkey.pub > etc/authorized_keys && chmod 600 etc/authorized_keys etc/testkey
/usr/sbin/sshd -f /root/sshtest/etc/sshd_config          # Port 2222, ListenAddress 127.0.0.1

cat > /root/.ssh/config <<'EOF'
Host testbox
  HostName 127.0.0.1
  Port 2222
  User root
  IdentityFile /root/sshtest/etc/testkey
  UserKnownHostsFile /root/sshtest/etc/known_hosts
EOF

# CRITICAL: both known_hosts must be absent before each run, or the discriminating
# condition is destroyed and the study measures nothing.
rm -f /root/.ssh/known_hosts /root/sshtest/etc/known_hosts

cargo build && cargo install herdr --version 0.1.0 --root /root/herdr-install
# then run each product in tmux 200x50 and execute tasks 1-7.
```
