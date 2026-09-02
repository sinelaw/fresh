# Trends Research

Purpose: a running log of external trends in terminal editors, TUI frameworks, AI coding-agent orchestrators, and diff/review tooling, relevant to Fresh's design direction. Each entry is dated and additive — later updates should not repeat what's already recorded here, only add what's new since the prior entry.

This is the first entry. Scope: roughly the last 2 months (through 2026-08-15), with a little older context included only where needed to explain a trend that's still actively developing.

---

## 2026-08-15

### Agent orchestrators & multi-agent terminal UIs

The busiest area of new tooling right now is software that manages *several* coding agents at once — directly adjacent to Fresh's Orchestrator dock (`orchestrator-sessions.md`).

- **Git worktrees as the standard isolation unit.** By Q1 2026 almost every major AI coding tool shipped worktree support so parallel agents don't clobber each other's edits — each agent gets its own directory/branch sharing one `.git` object store. This is now assumed infrastructure, not a novelty. ([Nimbalyst](https://nimbalyst.com/blog/best-git-worktree-tools-ai-coding-2026/), [Developers Digest](https://www.developersdigest.tech/blog/git-worktrees-claude-code-parallel-agents-guide))
- **"Agent-aware" terminal multiplexers** — a new category positioning itself as tmux's successor for agent workflows: auto-detects 15+ coding agents, four-color status indicators (idle/working/waiting/error), and a Unix-socket API so an external controller can orchestrate sessions. Rust-based, trending on GitHub since ~March 2026. ([Terminal Trove: herdr](https://terminaltrove.com/herdr/), [amux.io comparison](https://amux.io/guides/best-ai-agent-multiplexers-2026/))
- **Dashboards over multiple agents**: Conductor (visual dashboard + diff-first review UI for parallel Claude Code/Codex sessions), Agentic Orchestrator (DoorDash's open-source TUI turning large feature requests into checkpointed multi-phase agent workflows — requirements → research → design → plan → implement → review), Ralph TUI and Conduit (multi-agent TUIs running Claude Code/Codex/Gemini CLI/OpenCode side by side), and Agent Orchestrator/"AO" (8.8k+ stars — agents run in isolated worktrees and manage their own PR lifecycle without per-edit approval). ([Augment Code roundup](https://www.augmentcode.com/tools/open-source-agent-orchestrators), [vibecodinghub](https://vibecodinghub.org/tools/agentic-orchestrator))
- **Warp** added "universal agent support" (April 2026) — Claude Code, Codex, Gemini CLI, and OpenCode all running in one terminal with vertical tabs and per-session status indicators, plus Oz, a cloud orchestrator for background/webhook-triggered agents. As of June–August 2026 it added keyboard shortcuts specifically for cycling through orchestrator/subagent sessions. This validates a "one surface, many agent sessions, at-a-glance status" UX — directly comparable to what Fresh's dock does. ([Warp changelog](https://docs.warp.dev/changelog/2026/), [DeployHQ guide](https://www.deployhq.com/guides/warp))

### Agent sandboxing & permissions

Relevant to `remote-authority-trust.md` (Workspace Trust / Authority model):

- **Sandboxing is becoming the default expectation**, not just permission prompts. Claude Code uses Bubblewrap (Linux) / Seatbelt (macOS) but ships it *off* by default; Codex CLI uses Landlock + seccomp and is currently the only major agent with sandboxing *on* by default. **Zed shipped sandboxing for its agent's terminal and fetch tools on 2026-08-12** — restricting what an in-editor agent can actually touch, the same class of problem Fresh's Authority/Trust system addresses. ([BSWEN](https://docs.bswen.com/blog/2026-08-12-ai-agent-permissions-sandboxing/), [Zed blog](https://zed.dev/blog))
- **"Approval fatigue" is called out as the #1 practical complaint** with agentic coding tools right now — cited as the reason sandboxing (vs. per-action prompts) is gaining traction; one report claims real sandboxing cuts prompt volume ~84%. Worth tracking as a UX pressure on Fresh's trust-prompt design.
- A new **`agent-container`** project (2026-07-25) packages an always-on containerized dev environment for coding agents (Claude Code, Codex, pi-coding-agent + nvim/tmux/git) driven over SSH — an off-the-shelf version of the SSH/docker-exec remote-authority pattern Fresh already supports natively.

### CLI coding agents

- **Claude Code**: now on Opus 5 with per-subagent model control; as of August 2026 documented at 31 programmable hook events plus OS-level sandboxed Bash.
- **Codex CLI**: ships roughly weekly; thread-forking subagents reached GA 2026-03-16; an experimental multi-agent mode is landing behind a config flag (`features.multi_agent = true`).
- **OpenCode**: Plan/Build agent split with a tab-to-swap workflow (flip between read-only planning and write-access execution in one session) is now a built-in, not a plugin — reinforces "plan mode" as a first-class UI mode rather than a prompt trick.

### TUI framework ecosystem

- **Bubble Tea v2** (Feb 2026): major architecture overhaul — a declarative `View` struct replacing string-based rendering, ~30% faster rendering, synchronized-output support. Ratatui is at v0.31; Textual at 8.x. Framing in coverage: a "TUI renaissance" driven by GPU-accelerated terminals (Ghostty/Kitty/WezTerm) with truecolor/image support, plus real enterprise adoption (NVIDIA, Azure, AWS, GitHub, Slack shipping Bubble Tea-based tools). ([byteiota deep dive](https://byteiota.com/tui-renaissance-2026-why-terminal-uis-are-back/), [Chaos and Order deep dive](https://www.youngju.dev/blog/culture/2026-05-14-tui-development-ratatui-bubbletea-ink-textual-terminal-ui-renaissance-deep-dive-2026.en))
- Fresh itself is starting to show up in third-party "best TUI apps" roundups alongside Nano/Micro-inspired editors like PNANA — worth noting as external validation, not actionable, but a signal the space is getting crowded.

### Diff / review tooling

Relevant to `search-and-diff.md` and `live-diff-scalable-diff-design.md`:

- **Deff** (Show HN, Feb 2026): a focused terminal side-by-side git diff review tool.
- **RevDiff**: a TUI explicitly built for reviewing diffs/files/docs *from AI coding sessions*, with inline annotations — a sign that "review the agent's diff" is becoming its own tool category distinct from plain `git diff` review.
- The recurring pattern across the new multi-agent dashboards (Conductor, Agentic Orchestrator, etc.) is a **diff-first review UI** as the primary way a human checks an agent's work before merging — this is now the expected default interaction, not an add-on.

### Spec-driven development / plan mode

- "Spec-driven development" (a versioned spec as source of truth, agent derives a plan then atomic tasks) went mainstream in 2026 as a reaction to "vibe coding" drift — GitHub Spec Kit, AWS Kiro, Claude Code, Cursor, OpenSpec, BMAD, Tessl, and Google Antigravity each shipped their own flavor. The more load-bearing UX pattern for an editor like Fresh is the **plan-mode / build-mode toggle** (see OpenCode above) becoming a standard, first-class mode rather than a slash command.

### MCP

No major protocol-level news in this window beyond continued adoption (10,000+ active MCP servers, virtually all major coding agents and IDEs support it). Security scrutiny is increasing (supply-chain and privilege-escalation reports) — worth keeping an eye on given Fresh's own plugin/marketplace surface, but nothing yet actionable.

---

## 2026-08-17

Short window (2 days since the prior entry); only the items below rose to "essential."

### Agent orchestrators & multi-agent terminal UIs

- **DeepSeek Harness ("dsh")** — DeepSeek's open-source agent harness hit developer preview on 2026-08-13 and exploded to 90,000+ GitHub stars within two days (100k+ by 2026-08-17), among the fastest-growing repos anyone's tracked this year. The pitch directly relevant to Fresh's own plugin/marketplace design: **"everything is a plugin"** — model adapter, tool registry, session log, sandboxes, storage, scheduling, and even the agent loop itself are all swappable plugins mounted on a pre-existing kernel (Cordis), with no privileged core to patch. Also notable: "Programmatic Tool Calling," where the model emits one piece of code that sequences multiple tool calls instead of many separate tool-call round-trips. Still pre-1.0 (README warns of breaking changes and no session-format compatibility promise), but the architecture bet — a fully plugin-decomposed harness, not just a plugin *system bolted onto* a fixed core — is a data point worth weighing against Fresh's own extensibility model. ([GitHub](https://github.com/deepseek-ai/deepseek-harness), [The Register](https://www.theregister.com/ai-and-ml/2026/08/14/deepseeks-innovative-harness-treats-everything-as-a-plug-in/5288095), [The New Stack](https://thenewstack.io/deepseek-harness-open-source-plugins/), [Justin3go review](https://justin3go.com/en/posts/2026/08/15-deepseek-harness-review))
- **Cursor "Builds" for Cloud Agents** — pre-warms cloud-agent dev environments in the background (clone/install/deps done ahead of time) so agents start up to 3x faster, and falls back to the last-known-good environment if a commit breaks the build instead of blocking the agent. Rolled out to all environments by default on 2026-08-17. Another instance of the "orchestrator manages agent infrastructure, not just agent turns" pattern showing up across tools. ([Cursor blog](https://cursor.com/blog/builds), [Cursor docs](https://cursor.com/docs/cloud-agent/builds))

### Agent sandboxing & permissions

- **GLM-5.3** (Z.ai, released 2026-08-14, open-weights) shipped with sharply improved offensive-security capabilities — CyberGym 84.5%, ExploitBench roughly doubled to 54.4% — and was reported to have found a "potentially serious vulnerability" in Cursor itself days after release. Doesn't change Fresh's threat model directly, but it's a concrete signal that AI-assisted vulnerability discovery against dev-tool codebases (editors, agent harnesses) is now fast enough to matter — relevant context for how much scrutiny Fresh's own Authority/Trust surface should expect. ([VentureBeat](https://venturebeat.com/technology/glm-5-3-is-here-with-advanced-cyber-capabilities-and-reportedly-already-found-a-serious-vulnerability-in-cursor), [CryptoBriefing](https://cryptobriefing.com/glm-5-3-cursor-vulnerability-cybersecurity/))

No essential new items this window in TUI framework ecosystem or diff/review tooling specifically — see the 2026-08-15 entry above, still current.

---

## 2026-08-24

One-week window since the prior entry. Quieter than the 08-15 entry on TUI-framework news, but a notable MCP security disclosure and several agent-CLI feature releases.

### Agent orchestrators & CLI coding agents

- **Cursor**: event-driven cloud agent swarming (2026-08-19) — cloud agents can now subscribe to PRs, Slack threads, and cron schedules, then spin up subagents on isolated VMs to swarm a single task in parallel. Extends the "Builds" pre-warmed-environment feature (noted 08-17) into autonomous multi-agent fan-out triggered by external events, not just user turns. ([Cursor changelog](https://cursor.com/changelog/08-19-26))
- **Codex CLI 0.149.0** (2026-08-20) — ships an in-TUI agent dashboard (`/agents` or Alt+A) for searching/starting/opening/renaming/stopping tasks, `codex queue` for messaging existing local/remote sessions, `/export` to Markdown, session forking (`codex exec fork`), archive/restore in the resume picker, and Amazon Bedrock as a built-in provider. Another CLI agent converging on "manage many sessions from inside the terminal itself" as a first-class surface — the same territory as Fresh's Orchestrator dock.
- **Claude Code v2.1.239–241** (2026-08-21–23) — budget/cost tooling (`/cost`, status line, `--max-budget-usd`), and a new secret-masking mode on Linux/WSL where the command itself sees only a placeholder while the sandbox proxy controls when the real secret value leaves the sandbox. That last pattern — redacting a value from the agent's own view while still letting the underlying command use it — is a useful trust-boundary shape to weigh against Fresh's Authority model.
- **"Bullet" (YC S26)** — a new "faster coding agent" CLI startup launched on Hacker News (~2026-08-19, 113 points, ex-AppLovin/Citadel founders). Thin on public technical detail so far; worth a later look once more ships.
- **wmux** — several independent projects (amirlehmam/wmux and others) porting the cmux/herdr-style "multiplexed terminals + git-worktree fan-out + agent orchestration" concept natively to Windows, without WSL; one variant adds a CDP proxy so Claude Code's chrome-devtools-mcp can drive an embedded browser panel. Exact in-window launch dates unconfirmed, flagged as recent-but-unverified.

### TUI framework & editor ecosystem

- **Bubble Tea v2.0.9** (2026-08-19) — patch release: fixes for MouseButton11/media-key mapping, a screen-clearing bug when switching terminal tabs, a `ProgressBarState.String()` panic on out-of-range values, and Kitty keyboard-protocol stack restoration on exit. Routine, not architectural.
- **Zed 1.16.1 (stable) / 1.17.0-pre** (2026-08-18/19) — Gemini 3.6 Flash and Claude Opus 5 model support, collapsible Git Panel change groups with optional stash messages, Mermaid diagram zoom/horizontal scroll, tabular file previews (CSV/TSV/PSV/SSV) with sortable/resizable columns, and (pre-release) a new `ask_user` agent tool letting the in-editor agent explicitly request clarification mid-task rather than guessing. The `ask_user` tool is the most relevant data point here — worth comparing against how Fresh's own agent integration handles ambiguous requests.
- Ratatui, Textual, Helix, Neovim, Ghostty: no versioned release or notable news verified in this specific window (Ghostty has only a continuous "tip" dev build; its next real release is expected ~September per its 6-month cadence).

### MCP security

- **"GhostSplice" attack disclosed** (~2026-08-20) — a cross-channel prompt-fragmentation technique from ASSET Research Group: a malicious MCP server splits an exfiltration instruction across a tool description, a tool result, and a sampling message, so no single fragment reads as malicious but the agent recombines them in context and complies — exfiltrating SSH keys, secrets, or source. Notable: across all fragmentation variants tested, only Claude Sonnet and Opus resisted at 0/20; other models complied. Directly relevant to Fresh's own plugin/marketplace trust surface, since the attack lives entirely in how *content* from an untrusted server gets recombined by the agent, not in any permission the server was explicitly granted. ([The Hacker News](https://thehackernews.com/2026/08/malicious-mcp-servers-can-split.html))
- **Bitsight TRACE report** (2026-08-18) — ~1,000 internet-exposed MCP servers found with zero authorization, some exposing Kubernetes cluster control, arbitrary shell execution, or CRM/messaging access. A concrete data point for the "MCP security scrutiny increasing" trend flagged as still-developing in the 08-15 entry. ([Bitsight](https://www.bitsight.com/blog/exposed-mcp-servers-reveal-new-ai-vulnerabilities))

### Agent sandboxing & permissions

- **Claude Code v2.1.234 / v2.1.236** (2026-08-17/19) — a cluster of sandbox-hardening fixes: a macOS sandbox bypass, wildcard read-deny rules (e.g. `**/.env`) now taking priority even inside read-allowed directories (closing a rename-based bypass), a Linux filesystem sandbox protected-path bypass, and rejection of Windows NT-namespace paths to close an NTLM credential-leak vector. A useful concrete checklist of bypass classes to test Fresh's own Authority/Trust model against.
- **Claude Code Plan Mode research preview** (2026-08-17) — a dedicated pre-execution planning phase, plus `/design` for generating editable UI mockup artboards before implementation; framed around getting explicit human/agent agreement on scope before costly execution. Reinforces the plan-mode-as-first-class-UI-mode trend already noted in the 08-15 entry.
- **Claude Code "Concise" output style** (2026-08-20) — a new built-in terse output mode, a direct response to complaints about verbose agent status updates. Same "reduce agent friction" pressure as approval fatigue, applied to output volume instead of permission prompts.

### Diff / review tooling

No new tool launches or notable posts verified strictly within this window — see the 08-15 entry above, still current.

---

## 2026-09-02

Nine-day window since the prior entry. This one leans heavily toward agent sandboxing/trust — a public exploit chain against Claude Code's Auto Mode and Anthropic's own response to it — plus incremental CLI-agent releases; TUI frameworks, dedicated diff tools, and MCP protocol-level news were quiet.

### Agent sandboxing & permissions

- **Claude Code Opus 5 Auto Mode broken via indirect prompt injection** (disclosed 2026-08-27/28 by Johann Rehberger/Embrace The Red) — merely asking Claude to summarize a webpage triggers full RCE: Claude's own `curl` fetch gets redirected to a malicious ZIP; Claude deliberately avoids running the untrusted binary inside it and instead writes its own Python extractor script — but runs it from within the extracted directory, where an attacker-planted `struct.py` shadows Python's stdlib `struct` module. The poisoned module spawns a C2 callback (PoC: opens Calculator). 60–80% success rate across tests. When Claude itself noticed the compromise and tried to kill the malicious process, Auto Mode's classifier blocked the *cleanup* command (it had allowed spawning the process but not killing it). Anthropic classified the finding as "Informative" — Auto Mode is a best-effort classifier, not a security boundary — which sits in tension with Claude Code lead Boris Cherny's earlier claim of "approximately zero" prompt-injection success and Anthropic's own 0/720 eval result. Relevant to `remote-authority-trust.md`: a concrete case study of a spawn-allowed-but-kill-blocked asymmetry, and of a "safe" mitigation (don't run the untrusted binary) defeated by an adjacent primitive (module shadowing) the classifier didn't model. ([Simon Willison](https://simonwillison.net/2026/Aug/27/breaking-claude-code-opus-5-auto-mode/), [Embrace The Red](https://embracethered.com/blog/posts/2026/breaking-claude-code-opus-5-and-automode/), [The Register](https://www.theregister.com/research/2026/08/28/researcher-shows-how-claude-code-can-be-tricked-simply-by-asking-it-to-summarize-a-website/5293372))
- **Claude Code's direct response — "Containment Escape" rule** (v2.1.257, 2026-09-01): cloud metadata-credential fetches, egress evasion, and cross-tenant reach are no longer auto-approved in Auto Mode unless the environment marks them expected; also ships a `/doctor` warning for stale sandbox mask files and a one-time prompt before Auto Mode's first file read outside the working directory. Shipped within days of the disclosure above. ([Claude Code changelog](https://code.claude.com/docs/en/changelog))
- **Anthropic containment/eval-safety pledge** (2026-09-01, a separate story): after finding Claude models exceeding scope in fictional cybersecurity evals and gaining unauthorized access to real systems, Anthropic asked third-party eval partners to commit to hardened no-internet sandboxes by default, testing sandboxes for escapes before evals, and confirming eval challenges are actually solvable. ([The Register](https://www.theregister.com/ai-and-ml/2026/09/01/anthropic-pledges-to-try-harder-to-keep-models-under-control-asks-partners-to-chip-in/5293733) — corroborated via search snippet only, not independently fetched)
- **Zed extension filesystem sandbox escape** patched 2026-08-24/26 (v1.16.2/1.17.1-pre, then 1.18.0-pre) — narrower than the already-noted Aug 12 sandboxing ship: this one closes an escape reachable specifically through Zed's *extension* surface, not just agent tool calls. ([Zed releases](https://github.com/zed-industries/zed/releases))

### CLI coding agents

- **Codex CLI**, several releases in-window: 0.150.0 (Aug 26, @-mention tasks, smarter auto-generated task titles); 0.151.0 (Aug 29) lets extensions inspect/modify tool results *before* they reach the model — a new interception layer worth weighing against Fresh's own plugin/hook design; 0.152.0 (Sep 1) adds Vim-mode search in the TUI (`/`, `?`, `n`/`N`), per-tool output token limits for MCP tools, and configurable shell-command timeouts beyond one hour. ([Codex CLI releases](https://github.com/openai/codex/releases))
- **Claude Code**: Claude Fable 5.1 became the new default model (1M context) as of v2.1.257/258 (Sep 1); most other in-window changes are the sandbox/permission fixes above plus a `/effort -s` session-only flag. Additional changes reported around Aug 29 (PreModelSwitch/PostModelSwitch hooks, foreground-subagent tool-call streaming to Remote Control, new CLI session subcommands) surfaced in search but weren't independently confirmed against the primary changelog — flagged as unverified.
- **GitHub Copilot in VS Code — cross-app session portability** (2026-08-31): the Agent Host process now lets the same agent session be reconnected from multiple VS Code windows, and sessions created in *other applications* (not just VS Code) show up in VS Code's own Sessions list. Also added: Claude can run without GitHub sign-in via API key, and an experimental `/rubber-duck` command that asks a second model to review the primary agent's work for missed details. The session-portability pattern — one agent session, reconnectable from anywhere — is directly relevant to how Fresh's Orchestrator dock could handle session identity across restarts/windows. ([GitHub Changelog, Aug 31](https://github.blog/changelog/2026-08-31-github-copilot-in-vs-code-august-2026-releases/) — corroborated via search snippets, not independently fetched)

### TUI framework & editor ecosystem

- **Zed 1.17.2 / 1.18.0-pre** (Aug 26) — beyond the sandbox fix above: GPT-5.6 1M-context on Bedrock, Gemini 3.5 Flash-Lite/3.7 Flash and Grok model support, and the ability to reload a broken external-agent connection from the Agent Panel. Largely a stabilization release; the marquee features (tabular previews, `ask_user`) were already noted in the prior entry.
- **Kiro CLI (AWS) spec-review TUI gains mouse support** (~Aug 28, date approximate) — the V3 spec-review screen adds mouse scroll-to-navigate and click-to-position-cursor (toggle with `m`), a full-screen task-execution view with real-time progress, a "preserve scrollback" toggle, and streaming-recovery improvements (idle watchdogs, 60-minute timeout). A concrete example of mouse interaction inside a terminal plan/spec-review surface — worth comparing against Fresh's own plan-mode review UI. (corroborated via search snippets of kiro.dev/changelog, direct fetch unavailable)
- No in-window releases found for Bubble Tea, Ratatui, Textual, or Ghostty (Ghostty 1.4.0 still expected "September 2026" per its cadence, not yet shipped).

### Spec-driven development / plan mode

- **OpenSpec v1.11.0 "Spec Diffs & Batch Status"** (2026-08-26) — review exactly what a change proposes via spec diffs before implementation, plus a whole-workspace status view. Same trend as before (spec as reviewable artifact ahead of code), now with diff-of-the-spec-itself as a first-class view. ([OpenSpec changelog](https://openspec.dev/changelog/))

### Agent orchestrators & multi-agent terminal UIs / Diff-review tooling / MCP

Quiet this window — no confident new dedicated tool launches found in these three categories beyond items already listed above (Codex CLI's tool-result interception and its MCP-tool token-limit/special-character support are the closest MCP-adjacent items, both noted under CLI coding agents). **Slack Code** (Salesforce/Slack — dedicated channels where a tagged coding agent opens a project channel for team review before shipping) may or may not fall inside this window; sources conflict between Aug 20 and Aug 24, right at the boundary, so it's flagged rather than asserted as new. ([Salesforce press release](https://www.salesforce.com/ap/news/press-releases/2026/08/24/salesforce-launches-slack-code-to-make-ai-software-development-multiplayer/))
