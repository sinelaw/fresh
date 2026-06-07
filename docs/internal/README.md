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
| [REVIEW_DIFF_HUNK_PARITY_UX_DESIGN.md](REVIEW_DIFF_HUNK_PARITY_UX_DESIGN.md) | Forward-looking UX design for a v2 Review Diff that reproduces `hunk`'s look-and-feel (live split/stack/auto, file sidebar, syntax + word-level highlighting, context folds, bordered inline notes, `?`/menu/theme parity, file filter, watch, agent control) using Fresh primitives correctly (buffer-group panels, composite + virtual buffers, syntect diff highlighting, fold ranges, virtual lines, controls library), while keeping Fresh's staging/edit/export edge. Phased rollout + host-primitive list |
| [refactoring-planning-prompt.md](refactoring-planning-prompt.md) | LLM prompt for producing a refactoring plan in the shape of the existing `*-refactor-plan.md` docs |
| [buffer-refactor-plan.md](buffer-refactor-plan.md) | Plan to decompose `model/buffer.rs` into field-cluster sub-structs (`BufferFormat`, `BufferFileKind`, `Persistence`) |
| [global-search-ux.md](global-search-ux.md) | UX design to grow Live Grep into a universal "one-stop" search with a visible scope picker (project/ignored files, open + closed terminals, diagnostics, git history, worktrees, all Orchestrator sessions); ASCII wireframe alternatives + NN/g rationale + closed-terminal retention plan |
| [ORCHESTRATOR_DOCK_NNG_USABILITY_GUIDE.md](ORCHESTRATOR_DOCK_NNG_USABILITY_GUIDE.md) | NN/g-style usability test guide for the Orchestrator Dock — persona, goal-based task scenarios (T1–T10), severity scale, moderator script. Companion to the lower-level engineering checklist `dock-ux-test-plan.md` |
| [ORCHESTRATOR_DOCK_NNG_FINDINGS.md](ORCHESTRATOR_DOCK_NNG_FINDINGS.md) | Findings from running the dock usability test interactively (tmux): scorecard + severity-ranked defects (terminal shadows dock focus, dive lands in file tree, stale gutter on hide, …) with heuristics, evidence captures, and fixes |
| [PLAN-git-log-streaming.md](PLAN-git-log-streaming.md) | Plan to stream `git show` into a file-backed buffer (extend `spawnProcess` with `stdoutTo`; add lightweight `refreshBufferFromDisk`); eliminates 43 MB JS string + 1 M-entry FFI marshal on giant commits |
| [PLAN-git-log-diff-folding-and-highlighting.md](PLAN-git-log-diff-folding-and-highlighting.md) | Plan for incremental per-file/per-hunk folding (toggleable via standard `toggle_fold` key) and principled syntect-driven diff highlighting (extend `HighlightCategory` + theme bg keys; per-chunk through existing lazy-load), scalable to 2 GB diffs |
| [AUTHORITY_DESIGN.md](AUTHORITY_DESIGN.md) | The `Authority` pattern — the single backend slot ("where does the editor act?") behind which local / SSH / docker-exec filesystem + spawner + terminal wrapper all live; one per `Editor`, opaque to core, destructive transitions |
| [K8S_WORKSPACE_UX_DESIGN.md](K8S_WORKSPACE_UX_DESIGN.md) | Top-of-stack product/UX design for Cloud Workspaces: the durable "Workspace" mental model (state machine, pod-agnostic), end goals, the full F1-F11 user-flow catalog (onboarding, connect/resume, steady-state incl. port-forward, leave/stop/destroy, reconnect-after-reschedule, rebuild/resize, multi-workspace, hygiene, team distribution, failure recovery), and an 11-row UX alternatives/trade-off matrix with recommendations |
| [K8S_AUTHORITY_DESIGN.md](K8S_AUTHORITY_DESIGN.md) | Design for a Kubernetes `Authority` = the SSH remote-agent authority with a `kubectl exec` transport (reuses `RemoteFileSystem` / remote spawners / agent / reconnect via the transport-agnostic `AgentChannel`). Works against any cluster (EKS/GKE/AKS/k3d/minikube/kind). Storage is provider-specific — the AWS reference uses an **EBS GP3 live volume synced to S3** (not an S3 live mount — Mountpoint fails atomic saves), so Fresh's save path is unchanged. Adds agent-heartbeat + pod-reschedule reconnect for exec-session liveness |
| [K8S_WORKSPACE_PLUGIN_DESIGN.md](K8S_WORKSPACE_PLUGIN_DESIGN.md) | Design for the `k8s-workspace` plugin: bring-your-own-cluster pod management with a small `Provider` contract (`attach-existing` / `manifest` / `run` / `command`); the `command` provider is the escape hatch for Terraform/Helm/CDK/internal-CLI flows. Lifecycle state machine, config model, UX, cost/idle guardrails |
| [k8s-workspace-research-prompt.md](k8s-workspace-research-prompt.md) | A ready-to-run deep-research LLM prompt: best practices / common flows / pitfalls / pain points for cloud Kubernetes dev workspaces, ephemeral dev pods, S3/EFS/EBS storage trade-offs, cluster auth (IRSA/Pod Identity/access entries), Terraform-managed dev containers, and attach/cost UX |

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
