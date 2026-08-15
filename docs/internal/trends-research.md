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
