# Internal Documentation

This directory contains design documents, pending work tracking, and
architectural decision records for Fresh development.

## Key Documents

| Document | Description |
|----------|-------------|
| [design-decisions.md](design-decisions.md) | Unified audit trail of all major design decisions and trade-offs |
| [docs-audit-0.2.13.md](docs-audit-0.2.13.md) | Documentation gap analysis for 0.2.9–0.2.13 with validation checklist |
| [markdown.md](markdown.md) | Markdown compose mode remaining work |
| [code-review.md](code-review.md) | Code quality improvements to address |
| [refactoring-planning-prompt.md](refactoring-planning-prompt.md) | LLM prompt for producing a refactoring plan in the shape of the existing `*-refactor-plan.md` docs |
| [buffer-refactor-plan.md](buffer-refactor-plan.md) | Plan to decompose `model/buffer.rs` into field-cluster sub-structs (`BufferFormat`, `BufferFileKind`, `Persistence`) |
| [global-search-ux.md](global-search-ux.md) | UX design to grow Live Grep into a universal "one-stop" search with a visible scope picker (project/ignored files, open + closed terminals, diagnostics, git history, worktrees, all Orchestrator sessions); ASCII wireframe alternatives + NN/g rationale + closed-terminal retention plan |
| [ORCHESTRATOR_DOCK_NNG_USABILITY_GUIDE.md](ORCHESTRATOR_DOCK_NNG_USABILITY_GUIDE.md) | NN/g-style usability test guide for the Orchestrator Dock — persona, goal-based task scenarios (T1–T10), severity scale, moderator script. Companion to the lower-level engineering checklist `dock-ux-test-plan.md` |
| [ORCHESTRATOR_DOCK_NNG_FINDINGS.md](ORCHESTRATOR_DOCK_NNG_FINDINGS.md) | Findings from running the dock usability test interactively (tmux): scorecard + severity-ranked defects (terminal shadows dock focus, dive lands in file tree, stale gutter on hide, …) with heuristics, evidence captures, and fixes |
| [PLAN-git-log-streaming.md](PLAN-git-log-streaming.md) | Plan to stream `git show` into a file-backed buffer (extend `spawnProcess` with `stdoutTo`; add lightweight `refreshBufferFromDisk`); eliminates 43 MB JS string + 1 M-entry FFI marshal on giant commits |
| [PLAN-git-log-diff-folding-and-highlighting.md](PLAN-git-log-diff-folding-and-highlighting.md) | Plan for incremental per-file/per-hunk folding (toggleable via standard `toggle_fold` key) and principled syntect-driven diff highlighting (extend `HighlightCategory` + theme bg keys; per-chunk through existing lazy-load), scalable to 2 GB diffs |
| [AUTHORITY_DESIGN.md](AUTHORITY_DESIGN.md) | The `Authority` pattern — the single backend slot ("where does the editor act?") behind which local / SSH / docker-exec filesystem + spawner + terminal wrapper all live; one per `Editor`, opaque to core, destructive transitions |
| [EKS_S3_AUTHORITY_DESIGN.md](EKS_S3_AUTHORITY_DESIGN.md) | Design for a full cloud `Authority`: `S3FileSystem` (object store → POSIX `FileSystem` trait, ranged GET for lazy multi-GB loads, multipart `write_patched`, block-on bridge à la `AgentChannel`) + `EksExecSpawner` (`kubectl exec` into an EKS pod, modeled on `docker_spawner.rs`); mount-based path translation, no byte sync |

Individual design documents for specific features are preserved alongside
the unified summary for deep-dive reference.

## User-Facing Documentation

See the parent [docs/](../) directory:
- [Getting Started](../getting-started/) - Getting started guide
- [Features](../features/) - Editor features
- [Plugins](../plugins/) - Plugin system overview
- [Plugin Development](../plugins/development/) - Plugin development guide
- [Plugin API Reference](../plugins/api/) - Full plugin API reference
- [Architecture](../architecture.md) - System architecture
