# Session Transfer: Continue a Session in Another Agent or Place

Status: **design**. Nothing here is implemented yet. It builds on the Elsewhere
group (orchestrator-sessions.md §5.3a) and the Import dialog
(`agent_discovery.ts`), and uses their session model.

Purpose: from any session the dock knows about, whether a workspace or an
Elsewhere row, let the user pick up the work in another agent or on another
machine and carry on. Examples: a Claude session over SSH continued locally, a
Claude session continued in Codex or the reverse, or a Codex Cloud task brought
down into a local Claude.

The findings in §2 were checked against **Claude Code 2.1.284** and **Codex CLI
0.159.0**. Both CLIs were run against a local fake model endpoint in throwaway
config dirs (`CLAUDE_CONFIG_DIR`, `CODEX_HOME`, `HOME`), so no real session was
read, resumed, or teleported. Appendix A gives the method, so the checks can be
re-run when either CLI changes. Both on-disk formats are undocumented and change
between releases. Every claim below about them has a version attached.

---

## 1. Terms

- **Source**: the session being transferred: a workspace row, an Elsewhere
  row, or an Import dialog row.
- **Destination**: an *agent* (Claude, Codex) × a *place*. A place is this
  machine, an SSH host (a Fresh remote workspace), or the vendor's cloud.
- **Transfer** carries three things, and each has its own mechanism (§3):
  - the **conversation**
  - the **code state** (branch, commits, uncommitted changes)
  - the **settings**, such as auto mode
- **Fork vs move**:
  - *Fork*: the destination gets a new conversation id, and the original stays
    as it was.
  - *Move*: a fork, followed by stopping the original, and only when the user
    left that option on. Nothing here ever resumes the original id in a second
    process (§6.3).

---

## 2. What the CLIs support today (verified)

### 2.1 Claude Code (2.1.284)

- **Transcript**: `~/.claude/projects/<encodeProjectDir(cwd)>/<session-id>.jsonl`
  (`CLAUDE_CONFIG_DIR` replaces `~/.claude`). Each record is chained by
  `parentUuid`/`uuid`, and a trailing `last-prompt` record names the `leafUuid`.
  Besides the `user` and `assistant` records there are `attachment`,
  `queue-operation` and `cost-state` records, among others.
- **A hand-written transcript resumes.** Four records were enough: `user` text,
  then `assistant` `tool_use`, then `user` `tool_result`, then `assistant` text.
  Each record needs `parentUuid`, `uuid`, `sessionId`, `cwd`, `timestamp`,
  `type`, and a Messages-API `message`.
  - A file with those four records, placed in the bucket for the cwd, resumed
    with `claude -p --resume <id> "…"`.
  - The model request carried all four turns, with `tool_use`/`tool_result`
    intact, followed by the new prompt.
- **The bucket is not strict.** A transcript copied into another project's
  bucket with its `sessionId` rewritten, but its `cwd` fields untouched, also
  resumed.
  - `--resume <id>` from an unrelated cwd found the session too, in `-p` mode.
  - Staging into the destination's bucket is still the rule, because Claude's
    pickers and `--continue` go by the bucket.
- **Forking is native.**
  `claude --resume <id> --fork-session --session-id <new-uuid>` wrote
  `<new-uuid>.jsonl` holding the history plus the new turn, and left the
  original file byte-identical (same md5). This fits the orchestrator's
  *provision* strategy: Fresh mints the id, exactly as it does for
  `--session-id` today (orchestrator-sessions.md §8).
- **No importer for other agents' sessions.** `claude import [codex|gemini|cursor]`
  is described as importing *config*. In this build it prints "not yet
  available".
- **Cloud**:
  - `claude --teleport <id>` pulls a cloud session down (shipped in #3437).
  - `claude --cloud [description|id|url]` creates a cloud session, or attaches
    to one (account-gated).
  - `--remote-control [name]` starts a local session that can be driven from the
    web and phone apps.
  - There is no flag that uploads a *local* conversation into a cloud session.

### 2.2 Codex CLI (0.159.0)

- **Rollout**: `$CODEX_HOME/sessions/YYYY/MM/DD/rollout-<ts>-<uuid>.jsonl`, with
  an index in `$CODEX_HOME/state_5.sqlite` (`threads` table).
  - Records are `{timestamp, type, payload}`. The first is `session_meta` (`id`,
    `cwd`, `originator`, `cli_version`, `source`, …).
  - The conversation lives in `response_item` records (Responses-API items:
    `message`, `function_call`, `function_call_output`, `reasoning`).
  - Alongside them are `event_msg`, `turn_context`, `world_state` and
    `token_usage_record`.
  - New sessions record `history_mode: "paginated"`, and a
    `codex migrate-rollouts` command exists. The format is moving.
- **A hand-written rollout resumes.** Five records were enough: a minimal
  `session_meta`, then `response_item`s for user text, a `function_call`, its
  `function_call_output`, and assistant text.
  - The file was dropped into the dated directory and run with
    `codex exec resume <uuid> "…"`.
  - The model request carried all four items, followed by the new prompt.
  - Codex added the thread to its sqlite index on that first resume, so no
    index write is needed.
- **Codex imports Claude sessions natively.** The app-server protocol has
  `externalAgentConfig/detect` and `externalAgentConfig/import`, with a
  `SESSIONS` item type (`{path, cwd, title}`). It is reachable one-shot:

  ```sh
  (printf '%s\n' "$INITIALIZE" "$INITIALIZED" "$IMPORT"; sleep 4) | codex app-server
  ```

  - It replies `{importId}`, then sends `externalAgentConfig/import/progress`
    with `successes[].target` = the new thread id. It needed no experimental
    capability.
  - Result: a new rollout (`source: "vscode"`, `originator` = the client name)
    containing:
    - user and assistant text as messages
    - each Claude tool call and tool result **flattened to assistant text**
      (`[external_agent_tool_call: Bash] … [/external_agent_tool_call]`,
      `[external_agent_tool_result] … [/…]`)
    - attachments and system records dropped
    - a trailing `<EXTERNAL SESSION IMPORTED>` event
  - `codex exec resume <target>` then continues it with that history.
  - Codex records every import in `$CODEX_HOME/external_agent_session_imports.json`,
    with the source path, its sha256 and the thread id.
  - Constraints found:
    - The `path` must be a session Codex's own detection finds: under
      `$HOME/.claude/projects`, and within its default age limit. A copy
      elsewhere fails with `session_not_detected`.
    - The `cwd` parameter is **ignored**. The thread takes the transcript's
      recorded `cwd`, so a remapped copy has to be staged with its `cwd`
      rewritten (§4.3).
- **Resume and fork by id**:
  - `codex resume <uuid|name>`
  - `codex fork <uuid>`
  - `codex exec resume|fork` (non-interactive)
  - Codex has no `--session-id` to mint an id at launch.
- **Cloud**:
  - `codex cloud list|status|diff|apply <task>`
  - `codex cloud exec --env <ENV> [--branch B] "prompt"`, which creates a new
    task on a *pushed* branch
  - A cloud task's conversation cannot be downloaded. Only its status and diff
    can.

---

## 3. What a transfer carries

### 3.1 The conversation: three tiers

1. **Native.** For the same agent, copy or fetch the transcript and fork it with
   the agent's own resume: Claude `--resume … --fork-session --session-id`,
   Codex `fork`. This is lossless.
2. **Translated.** For the other agent, a new native transcript is written from
   the source's messages, and the destination resumes it as if it were its own.
   It is lossy by construction:
   - Tool calls are rendered as text. Replaying them as structured calls would
     name tools the destination does not have (`Bash`, `Edit` versus `shell`,
     `apply_patch`). Codex's own importer makes the same choice.
   - Reasoning is dropped. Encrypted reasoning cannot cross vendors, and
     Claude's thinking blocks are signed.
   - Images, attachments, subagent sidechains and per-turn metadata are
     dropped.
   - Long tool outputs are truncated, with the cut marked in the text.
3. **Handoff.** No transcript reaches the destination as history. The destination
   starts fresh, with a *start prompt* that points at a handoff file. That file
   holds a structured summary (goal, what was done, open items, branch, how to
   verify) and the full transcript rendered as Markdown. This is the only option
   when:
   - the conversation cannot be read (Codex Cloud tasks)
   - the destination creates sessions only from a prompt (Claude `--cloud`,
     `codex cloud exec`)
   - the translated history would not fit the destination's context window (the
     threshold is an open question)

### 3.2 Recommendation: translation plus a transfer note, handoff as fallback

- **Translation, not handoff alone, is the default between agents.**
  - A resumed conversation keeps the user's exact words, the decisions made and
    the dead ends, and the user can scroll back through them in the
    destination's own UI.
  - A summary is lossy in ways nobody can check, and it costs a model call.
  - Both CLIs were shown to accept a written transcript (§2), and the text-only
    form has the smallest attack surface in either format.
- **Claude → Codex uses Codex's importer first**
  (`externalAgentConfig/import`).
  - It is the vendor's own translation, and it keeps working as the rollout
    format moves (paginated history, sqlite index).
  - It records the import in Codex's ledger.
  - Fresh's own rollout writer is the fallback. It is used for a Codex older
    than the method, for a session outside Codex's detection window, and for
    SSH destinations, where Fresh writes the file remotely. It emits the same
    text-flattened shape.
- **Codex → Claude uses Fresh's writer.** Claude has no session importer.
- **Every translated transfer carries a transfer note.** The destination starts
  with a start prompt (Codex `resume <id> "<note>"`, Claude
  `--resume <id> "<note>"`).
  - The note says the conversation came from another agent, and that tool calls
    above it are text, not calls that ran here.
  - It names the working directory, the branch, and whether the tree carries
    uncommitted changes from the source.
  - It gives the path of the full handoff file, for anything the translation
    dropped.
- **Handoff is used where translation is impossible or too large.** It is never
  a silent substitute: the form says which tier the transfer will use.

### 3.3 The code state

- **Same machine, new worktree** (the default). Fresh copies the source tree's
  state without touching the source's index or working tree (checked):

  ```sh
  T=$(mktemp); cp "$(git rev-parse --git-path index)" "$T"
  TREE=$(GIT_INDEX_FILE=$T sh -c 'git add -A && git write-tree'); rm "$T"
  SNAP=$(git commit-tree "$TREE" -p HEAD -m "fresh transfer snapshot")
  git worktree add [-b <branch>] <dest> HEAD
  git -C <dest> read-tree -u --reset "$SNAP" && git -C <dest> reset -q
  ```

  - The destination sits on the source's HEAD, with the same modifications,
    deletions and untracked files as uncommitted changes.
  - Ignored files do not come across, and the staged/unstaged split flattens to
    unstaged.
  - The snapshot commit is an unreferenced object and is garbage-collected
    normally.
  - `git stash create` is not used, because it skips untracked files.
- **Same machine, in place.** The destination runs in the source's own root.
  Only a *move* can do this: two agents must not share one working tree, so the
  original is stopped first (§6.3). Nothing is copied.
- **SSH host → here** and **here → SSH host**: §5.
- **Cloud**: §4.4. Code reaches a cloud session only through a pushed branch,
  which needs the user's explicit action (§6.2).

### 3.4 Settings, tools and permissions

| Thing | Carried? |
| --- | --- |
| Auto mode | Yes, as the form's **Auto mode** checkbox, mapped through each agent's registry flag (`claude --permission-mode auto`, `codex --full-auto`). The source's posture is the default when it is known (workspace rows). |
| Model, effort | No. The destination uses its own defaults. |
| Per-project permissions (`.claude/settings.local.json` and the like) | Only when the snapshot happens to carry them. These files are usually git-ignored, so a new worktree does not get them. The form says so when the source has such a file. |
| MCP servers, skills, hooks, plugins | No. These are the user's agent configuration, not the session's. Codex's own migration (`externalAgentConfig/import` with the other item types) is left to the user. Fresh imports `SESSIONS` only. |
| Background jobs, subagents, todo and plan state, Claude's rewind checkpoints | No. They die with the conversation's runtime. |

---

## 4. Matrix

The rows are sources and the columns are destinations. "Here" means this
machine, "SSH" means a Fresh remote workspace on a host, and "Cloud" means the
vendor's cloud.

| Source ↓ / Dest → | Claude here | Claude SSH | Claude cloud | Codex here | Codex SSH | Codex Cloud |
| --- | --- | --- | --- | --- | --- | --- |
| **Claude here** (workspace, terminal, `--bg`, Desktop) | N: fork | B: copy + fork | H | T: Codex import | T: Fresh writer | H |
| **Claude over SSH** (Desktop SSH, Fresh SSH workspace) | B: fetch + fork | N on that host / B elsewhere | H | B+T | T | H |
| **Claude cloud / Remote Control** | N: teleport (#3437) | N: teleport run on the host | — | N+T: teleport, then as "Claude here" | N+T | H |
| **Codex here** | T: Fresh writer | T | H | N: `fork` | B: copy + fork | H |
| **Codex over SSH** | B+T | T | H | B: fetch + fork | N on that host | H |
| **Codex Cloud task** | C+H | C+H | — | C+H | C+H | — |

Legend:
- **N**: native mechanism, lossless.
- **B**: buildable with Fresh plumbing: moving files and git over SSH.
- **T**: translated (§3.1), lossy in the documented ways.
- **H**: handoff only. A new session is seeded from a prompt, and the code
  arrives only after the branch is pushed, with the user's consent.
- **C**: code only, through `codex cloud apply`/`diff`.
- **—**: not offered (cloud to cloud).

Notes on specific cells:

- **Claude here → Claude here.** The conversation step is
  `claude --resume <src> --fork-session --session-id <new>`, run in the
  destination root. A move in place (same root, original stopped) could use
  plain `--resume <src>`. Forking anyway costs nothing and keeps the original
  file untouched, so a transfer always forks.
- **Codex here, as a source.** The Elsewhere row has only a pid and a cwd
  (orchestrator-sessions.md §5.3a gaps).
  - The rollout is found by cwd: the newest rollout whose `session_meta.cwd` is
    the row's cwd. The form shows that thread's first user message for the user
    to confirm, since the match is inexact, just like `codex resume --last`.
  - A workspace Fresh started knows the thread once §7.1 records it.
- **Claude cloud → anything.** Teleport comes first: it is the only way to get
  the conversation. Teleport is itself a move: the cloud session hands over to
  the local checkout. Anything after it is a second, local transfer. The two
  steps run as one action, "Continue in Codex", and the form says there are two
  steps.
- **Remote Control rows.** The session runs on one of the user's own machines. If
  that machine is this one, the row is a local session and needs no teleport.
  Whether teleport accepts a bridge session on another machine is an open
  question (§9).
- **Codex Cloud task → here.**
  - `codex cloud apply <id>` runs in a fresh worktree, cut from the task's base
    branch where `codex cloud status` gives one.
  - The conversation cannot be downloaded, so the destination gets a handoff
    built from the task's title, prompt and status, plus the path of the
    applied diff.
- **Here → cloud (either vendor).** The conversation cannot be uploaded, so the
  form offers two routes:
  - **Keep it local but reachable.** For Claude, the session is resumed in place
    with `--remote-control`, which is the supported way to drive a local session
    from the web or phone. For Codex, `codex remote-control` plays the same role,
    but it is experimental and left out of the first versions.
  - **Start a cloud session from a handoff.** This runs `claude --cloud "<handoff>"`
    or `codex cloud exec --env <E> --branch <B> "<handoff>"`. The form has this
    route disabled until the branch is pushed. The push is a separate, explicit
    checkbox, off by default (§6.2).

---

## 5. SSH ↔ here

### 5.1 Which SSH sessions

- **Claude Desktop SSH rows.** `LiveSession.sshHost` comes from Desktop's
  `sshConfig`, and the CLI session id from `cliSessionId`. The transcript lives
  on that host.
- **Fresh SSH workspaces.** The window's `RemoteAgent` authority already runs
  commands and reads files there.

### 5.2 SSH → here

1. **Fetch the transcript** to a staging file under
   `<data>/orchestrator/transfers/<xfer-id>/`.
   - A Fresh SSH workspace reads it with `editor.openMachine({kind:"window",…})`
     and `readFilePrefixes` (a large `maxBytes`). That path already goes through
     the authority, and no new ssh process is spawned.
   - A Desktop SSH row runs `ssh -o BatchMode=yes <host> sh -c '…cat…'` through
     `spawnHostProcess`, with argv only. The remote script resolves
     `${CLAUDE_CONFIG_DIR:-$HOME/.claude}/projects/<encodeProjectDir(cwd)>/<id>.jsonl`
     on the host. `BatchMode` makes a host that wants a password fail at once
     with a clear message, rather than hang an invisible prompt. That host is
     then listed as "needs key-based SSH".
2. **Fetch the code.** No shared remote is needed. The host's repository is a
   remote in its own right:
   - Fresh asks the host for `git -C <cwd> rev-parse HEAD`, its branch, and
     `remote get-url origin`.
   - The form proposes as Project Path an open workspace whose origin matches,
     or else asks for the local checkout.
   - On Create, Fresh runs the §3.3 snapshot on the host, which writes one
     object and a temporary ref `refs/fresh-transfer/<xfer-id>`.
   - Locally it runs `git fetch ssh://<host>/<cwd> refs/fresh-transfer/<xfer-id>`,
     then creates the worktree from `FETCH_HEAD`'s parent with the snapshot laid
     over it (§3.3).
   - It then deletes the temporary ref on the host. The ref is the only write on
     the host. It never touches the host's index, working tree or branches, and
     the form names it.
3. **Stage the transcript.** Fresh rewrites `sessionId` to the new uuid, and
   every `cwd` to the local root (the same string replacement on
   `gitBranch`/`cwd`, and nowhere in message text). It then writes the file to
   `~/.claude/projects/<encodeProjectDir(localRoot)>/<new>.jsonl`, using
   `writeFile`, which never overwrites.
4. **Launch.** The destination agent runs in the new workspace:
   - Claude: `claude --resume <new>`. The staged file is already the fork, so no
     `--fork-session` is needed.
   - Codex: import the staged file (§2.2), then `codex resume <target> "<note>"`.

The remote conversation is left as it was. If it is still running there, the
local copy stops at the fetched point. The form shows the transcript's
last-activity time so the user can see that.

### 5.3 Here → SSH

This is the mirror of §5.2, and the destination must be a Fresh SSH workspace.
Fresh does not write into Claude Desktop's session store: it is Desktop's file,
and there is no documented way to register a session in it.

1. **Push the code** over SSH to the host's checkout:
   `git push ssh://<host>/<repo> <snap>:refs/fresh-transfer/<xfer-id>`.
   - The host then creates a worktree from it, and the ref is deleted.
   - This is a push, but to the user's own machine and to a private ref, never
     to a shared remote. The form still states it, and the Create button is the
     consent.
   - A host with no checkout of the repository gets
     `git clone ssh://<this-machine>…`. That route is out of scope, since a
     laptop is usually unreachable from the host. Instead the form asks the user
     for a checkout path on the host.
2. **Copy the transcript** into the host's bucket (Claude), or write a translated
   or copied rollout into the host's `$CODEX_HOME/sessions/YYYY/MM/DD/` (Codex).
   The copy goes through the workspace authority. Codex's importer is not used
   on a remote host in the first versions; the host would need a new enough
   Codex and a detected path.
3. **Launch** the resume argv through the authority, as §8 of
   orchestrator-sessions.md already does for remote agent terminals.

---

## 6. Safety rules

### 6.1 Credentials

- **No token is ever put on a command line or into a file Fresh writes.**
- **Transfer adds no credential use.** Claude cloud listing already sends its
  token as a header only (orchestrator-sessions.md §5.3a).
  - SSH uses the user's own agent and config.
  - `codex app-server` runs with the user's `CODEX_HOME` and needs no
    credential to import.
  - Teleport and `--cloud` are run as the user's own CLI in a terminal.
- **The one-shot import script sends JSON-RPC over stdin.** The requests hold
  paths and ids only. They go into a file under the transfer's staging
  directory. The script pipes that file into `codex app-server`, then holds
  stdin open until the output shows `externalAgentConfig/import/completed`, or
  until a timeout. Holding stdin open matters because the import completes
  asynchronously, after its reply. The requests never appear in argv.

### 6.2 Nothing published without the user's action

- **No push to a shared remote, ever, without its own checkbox.** Every such
  checkbox is off by default and names the branch and the remote. Only the
  "start a cloud session" route has one.
- **Private-ref pushes to the user's own SSH host show on the form.** Creating
  the workspace is the consent for them (§5.3). The same holds for the
  temporary ref on a source host (§5.2).
- **Cloud sessions and tasks are only created by the final Create.** Nothing
  creates them from a menu click alone.

### 6.3 The original is never destroyed implicitly

Four rules:

1. **Always fork.** The destination runs under a new conversation id (Claude
   `--fork-session`, Codex import/fork/copy under a new uuid). The source's
   transcript file is never written to. This is what rules out two writers on
   one conversation. A second resume of the *same* id is never issued, whether
   the source is running or not.
2. **Stopping is opt-in and comes last.** The **Stop the original** checkbox is
   the difference between a move and a fork. It is:
   - *on* for a workspace row, whose agent Fresh owns, stops with
     `signalTerminal`, and can relaunch
   - *on* for a Claude `--bg` job, via `claude stop <job>`, which keeps the
     conversation
   - *absent* for anything Fresh cannot stop cleanly: another terminal's
     process, Desktop, cloud, an SSH host
   It runs only after the destination has started. A failed transfer never
   stops anything.
3. **Nothing is removed.** Fresh never deletes a transcript, never archives or
   removes a worktree, and never runs `claude rm`. The original workspace stays
   in the dock, stopped, and its own Archive and Delete remain the user's to
   use.
4. **A source that is still working gets a warning.** If the source's state is
   `working`, the form says the copy is taken now and will not include what the
   source does next. It suggests waiting, or ticking Stop. "Working" is the
   dock's heuristic (orchestrator-sessions.md §4.4), so the form only warns and
   does not block.

In-place moves (§3.3) stop the original *first*, since they share a tree.
Fresh then checks that the agent's process group is gone before launching the
destination. If it cannot confirm that, it falls back to a new worktree.

---

## 7. UX in the dock

### 7.1 Prerequisite: workspaces remember their agent session

- **Today a workspace row cannot say which conversation it holds.**
  `resolveAgentLaunch` mints the Claude uuid and hands `{command, resume}` to
  `createWindowWithTerminal`, but nothing the plugin can read back keeps it.
- **Fresh adds** `setWindowState("agent_session", {agent, id?, resume})` at
  create time, next to `project_path`/`shared_worktree`. It is persisted in
  `session_plugin_state`.
  - `id` is present for provisioned agents (Claude).
  - For Codex, `id` is filled in after launch, by matching the newest rollout
    whose `session_meta.cwd` is the root and whose timestamp is after the
    launch. This also gives Codex workspaces an exact `codex resume <id>`
    instead of `--last`.
- **Older workspaces fall back to the same discovery** by root:
  `claude agents --json` or the registry by cwd, then the newest transcript in
  the root's bucket. The form shows the first user message of whatever was
  found, so the user can confirm it.

### 7.2 The action

- **Where it appears.**
  - The context menu of a workspace row, an Elsewhere row, and (later) an Import
    dialog row gets **Continue in…**.
  - The palette gets `Orchestrator: Continue In…`, which acts on the selected
    row.
  - On a Claude cloud row it sits under the existing Take Over Here
    (Teleport)… and Open in Browser.
- **Rows that cannot be transferred show it disabled, with the reason.** A
  Codex process whose rollout cannot be found, for example.

**Destination picker.** It is a small list, in the style of the existing menus,
and lists every destination §4 allows for the source:

```
Continue "fix flaky test" in…
  Claude · here            new worktree · full history
  Codex  · here            new worktree · history translated
  Claude · devbox (ssh)    history + code copied to devbox
  Claude · cloud           handoff only · needs branch pushed
```

- **Hosts** are the saved machines the New Workspace form already knows (its
  `FormSeed` options).
- **The second column** is the tier (§3.1) and the one cost the user should
  know about before choosing.

### 7.3 The form

Choosing a destination opens the **New Workspace form**, through the same
`openWorkspaceForm(seed, prefill)` path Take Over Here uses. The seed is the
destination machine, and the prefill fills the fields below.

| Field | Value |
| --- | --- |
| Project Path | The source's repository, or a matching open workspace (for SSH, matched by origin URL). |
| Create worktree | On. Off is offered only for an in-place move on this machine. |
| Branch | The source's branch, when the destination should share it. Otherwise a new `<branch>-<agent>` name. |
| Agent | The destination agent (locked). |
| Command | Shown read-only: `claude --resume <new>` / `codex resume <target>`. |
| Folder | The source's folder (`intoFolder`), so the new workspace files next to the old one. |

It also shows the transfer lines:

- **Conversation**: "full history" / "translated (tool calls as text)" /
  "handoff only".
- **Code**: "copies uncommitted changes (N files)" / "fetches from devbox" /
  "code not transferred until pushed".
- **Stop the original** (checkbox, §6.3).
- For cloud routes: **Push `<branch>` to `origin`** (checkbox, off).
- Warnings: the source is still working; per-project settings are not carried.

Two details:

- **Pending state until Create.** The prefill holds only a pending transfer
  record, and nothing is staged before the user presses Create. Cancelling
  leaves no trace.
- **The prefill grows one field.** `openWorkspaceForm`'s prefill gets
  `transfer?: TransferPlan`, and the form renders the lines above from it. The
  existing `cmd` stays the single source of the argv shown, which keeps the
  prefill path used by Take Over Here and Import unchanged.

### 7.4 Execution, status and errors

A transfer is a list of steps built by a pure planner (§8):

```
plan: snapshot → worktree → stage-conversation → create-window → launch → (stop-original)
```

- **Progress.** Each step reports to the status bar ("Continue in Codex: staging
  conversation…"). The new workspace row shows `pending`, as today's creation
  flow does.
- **Transfer record.** Each transfer writes one JSON file,
  `<data>/orchestrator/transfers/<xfer-id>.json`:
  - source key, destination, snapshot sha, new ids, and step results
  - the dock row's detail line reads "continued from Claude · fix flaky test"
  - it is the evidence for a bug report
- **On failure:**
  - The failing step's message goes to the status bar and the record.
  - Only what this transfer created is rolled back: its staged files, and the
    worktree if nothing has been written to it since. The original is never
    touched.
  - The pending row offers **Retry** and **Dismiss**, like the windowless
    pending placeholder.
- **Idempotence.** Staging writes with `writeFile`, which refuses to overwrite.
  A retry reuses the transfer's own ids, so a half-finished transfer cannot
  create a second copy.

---

## 8. Code shape

- **`plugins/lib/session_transfer.ts`**: pure, no editor calls.
  - `destinationsFor(source: TransferSource, machines): Destination[]`: the §4
    matrix as data, with a tier and a disabled reason.
  - `planTransfer(source, dest, opts): TransferPlan`: the step list plus the
    argv for each step. It uses `encodeProjectDir`, the registry's resume and
    auto flags, and `resumeArgv`.
  - `claudeChain(jsonl: string): Turn[]`: walks `parentUuid` back from the leaf
    (the `last-prompt` `leafUuid`, else the last record). It skips
    sidechains and non-message records, and starts at the last compaction
    boundary when there is one.
  - `codexItems(jsonl: string): Turn[]`: reads `response_item`s. `reasoning` is
    dropped.
  - `renderClaudeTranscript(turns, {sessionId, cwd, now}): string` and
    `renderCodexRollout(turns, {id, cwd, now}): {relPath, text}`: the Fresh
    writers, text-flattened (§3.2).
  - `remapClaudeTranscript(jsonl, {sessionId, cwd}): string`: for native copies
    (§5.2 step 3).
  - `renderHandoff(turns, meta): string`: the Markdown handoff file and the
    transfer note.
- **`plugins/orchestrator.ts`**:
  - the menu items
  - `openTransfer(sourceKey)`, which shows the picker and then opens the form
    with the plan
  - `runTransfer(plan)`, which runs the steps through `spawnHostProcess` /
    `spawnProcess` (per authority), `openMachine`, `writeFile`, and
    `createWindowWithTerminal`
  - the `agent_session` window state (§7.1)
- **`plugins/live_sessions.ts`**: exports a transcript locator for its rows,
  meaning the path, and the host for SSH. The parsers already carry `sshHost`,
  `cwd` and the CLI id.

---

## 9. Phased plan

### Phase 1: same machine, Claude ↔ Codex, workspaces and local rows

Scope:
- §7.1 `agent_session` window state, with discovery fallback.
- Sources:
  - workspace rows
  - Claude local Elsewhere rows (stopped, `--bg`, Desktop non-SSH)
  - Codex local rows (cwd match, confirmed)
- Destinations: Claude here, Codex here, in a new worktree (§3.3 snapshot).
- Conversation:
  - Claude → Claude: fork.
  - Claude → Codex: Codex importer, with the writer as fallback.
  - Codex → Claude: writer.
  - Codex → Codex: `codex fork`.
  - Every translated transfer carries the transfer note.
- Stop the original, for workspace rows and `--bg` jobs only.

Tests:
- **Unit tests** (`plugins/tests/session_transfer.test.ts`, run by
  `plugins/tests/run.sh`):
  - `destinationsFor` for every source kind
  - chain walking: branches, sidechains, compaction
  - both writers, round-tripped through the readers
  - `remapClaudeTranscript` leaves message text untouched
  - `planTransfer` argv: no token-bearing strings, and `writeFile`-only staging
  - the handoff renderer's truncation
  - The fixtures are small hand-written transcripts, shaped like the records in
    §2 and scrubbed of any real content.
- **e2e tests** (`crates/fresh-editor/tests/e2e/orchestrator_transfer.rs`),
  modelled on `orchestrator_elsewhere.rs`:
  - The fake CLIs are `bin/fake-claude` and `bin/fake-codex`, set as
    `claudeCommand`/`codexCommand`:
    - `fake-claude` logs its argv and prints `RESUMED-<id>`.
    - `fake-codex` answers `app-server` by reading stdin and printing a canned
      `import/progress` with a `target`. It answers `resume <id>` with
      `RESUMED-<id>`.
  - The tests assert on:
    - the menu item
    - the picker
    - the form's transfer lines
    - the new worktree's `git status`, which matches the source's uncommitted
      changes while the source tree is unchanged
    - the staged transcript file under a test config dir
    - the terminal text `RESUMED-<new id>`
    - the original workspace left running, or stopped when ticked
  - **Blocker:** the e2e harness has no fake `HOME`. The live_sessions readers
    and the new staging code would touch the real `~/.claude` and `~/.codex`.
    Phase 1 therefore adds `claudeConfigDir` and `codexHome` plugin settings,
    which default to the CLIs' own resolution (`CLAUDE_CONFIG_DIR`,
    `CODEX_HOME`). The e2e tests point both at the test root. This also closes
    the same leak in `orchestrator_elsewhere.rs`.
- **Real-CLI check, opt-in, not in CI:** the Appendix A procedure as a script,
  so a CLI upgrade that breaks the formats is caught by running one command.

### Phase 2: SSH

- Sources: Claude Desktop SSH rows (`ssh -o BatchMode=yes`) and Fresh SSH
  workspaces (the authority).
- Destinations: here, and a Fresh SSH workspace.
- Code over `ssh://` with temporary private refs (§5).
- e2e: a fake `ssh` on `claudeCommand`'s pattern, with a `sshCommand` setting,
  that runs its remote command locally in a second temp dir. The git steps then
  run for real against a local "remote" path.

### Phase 3: cloud

- Claude cloud → Codex: teleport, then transfer, as one action.
- Codex Cloud → here: `cloud apply` plus handoff.
- Here → cloud:
  - Claude: resume in place with `--remote-control`.
  - Both vendors: a cloud session from a handoff (`claude --cloud`,
    `codex cloud exec`), behind the push checkbox.
- Import dialog rows as sources.

---

## 10. Open questions

- **Context-size threshold.** Past what size of translated history does the form
  switch to handoff? Claude compacts on its own when resuming a long
  conversation, and Codex's behaviour on an oversized imported rollout is
  untested. A first cut: estimate tokens at chars/4 and switch at half of the
  destination model's window.
- **Codex importer age limit.** `detect` takes `maxSessionAgeDays`, but `import`
  re-detects with the default. What is the default? And does an old but
  explicitly named session fail? If it does, the fallback writer handles it.
  The planner needs to know in advance, so that the form's tier line is right.
- **Remote Control rows on another machine.** Can `--teleport` take over a bridge
  session? If not, the only transfer from such a row is at its machine.
- **`--remote-control` with `--resume`.** Is the combination supported for the
  "keep it local, reachable" route? The help lists both, but the pair is
  untested.
- **Codex Desktop app threads.** Their rollouts live in the same store
  (`originator` names the app), so they are transferable as sources. Stopping
  them is not possible, so they are always forks.
- **Translated fidelity.** Should Fresh's writers emit structured tool calls when
  the destination has an equivalent tool (`Bash` ↔ `shell`)? Text is safer and
  matches Codex's importer. Revisit only if resumed agents visibly misread the
  flattened history.
- **The transfer note's wording.** The note steers the destination agent. It
  should be short, factual, and the same for every pair. It needs a review
  pass, and it needs i18n only if it is shown in the UI, which it is not.

---

## Appendix A: How the §2 findings were checked

1. A ~60-line Node server on `127.0.0.1` answered both
   `POST /v1/responses` (OpenAI Responses SSE) and `POST /v1/messages`
   (Anthropic SSE) with a canned reply, and logged every request body.
2. Codex used a throwaway `CODEX_HOME` whose `config.toml` sets
   `model_provider = "fake"` with `base_url = "http://127.0.0.1:<port>/v1"`,
   `wire_api = "responses"`, and `env_key` naming a dummy variable.
3. Claude ran under `env -i`, with a throwaway `HOME` and `CLAUDE_CONFIG_DIR`,
   `ANTHROPIC_BASE_URL` pointing at the server, and a dummy
   `ANTHROPIC_API_KEY`. `env -i` keeps the calling session's environment out.
4. Each claim in §2 is one run whose logged request shows which history the
   model received:
   - `codex exec`
   - `codex exec resume`
   - `claude -p`
   - `claude -p --resume`
   - `--fork-session`
   - the `codex app-server` import pipeline
5. The written files were inspected with a short Python reader.
