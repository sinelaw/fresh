# Deep-research prompt — cloud Kubernetes developer workspaces

Hand the prompt below to a research-capable LLM (web access on). It is
written to gather the real-world best practices, common user flows, and
pain points that should shape the `k8s-workspace` plugin and the EKS
authority. Findings feed back into
[`K8S_WORKSPACE_PLUGIN_DESIGN.md`](K8S_WORKSPACE_PLUGIN_DESIGN.md) and
[`K8S_AUTHORITY_DESIGN.md`](K8S_AUTHORITY_DESIGN.md).

---

## PROMPT

You are a senior platform engineer and developer-experience researcher.
I am designing a feature for a terminal code editor that lets a developer
edit code "inside" an ephemeral Kubernetes pod running on **their own
Amazon EKS cluster, in their own AWS account**. The editor attaches to a
running pod (over `kubectl exec`, bootstrapping a small agent) and treats
that pod as the workspace. The pod mounts the source tree from a durable,
cheap tier (an **S3-backed volume**, e.g. Mountpoint for Amazon S3 CSI),
so that when the pod is torn down the code still lives in S3, and a new
pod re-mounting the same bucket restores the workspace. A companion
editor plugin lets users **bring pods up/down in a fully customizable
way** — built-in helpers (`kubectl apply` of a manifest, `kubectl run`)
plus an escape hatch that runs *any* user command/script (so teams who
provision "VDI-style developer containers" via **Terraform in a separate
repo**, Helm, Pulumi, CDK, or an internal CLI can plug their flow in).

Research how teams actually do this today and where it hurts. Prioritize
**primary, recent (last ~2-3 years), and authoritative** sources: AWS
docs and blogs, Kubernetes docs, project docs/GitHub issues for the
tools below, conference talks, and detailed practitioner write-ups.
**Distinguish vendor marketing from independent experience reports.**
For every non-obvious claim, cite the source and note its date; flag
anything that may be outdated or version-specific. Call out where
sources disagree.

### Questions to answer

**1. Remote / cloud dev environment landscape & expectations**
- How do existing cloud-dev products work and what do they get praised
  or criticized for: GitHub Codespaces, Gitpod (incl. the move to Gitpod
  Flex / "bring your own cloud"), Coder, DevPod, Okteto, Telepresence,
  `devcontainer` CLI, JetBrains Gateway/Space, VS Code Remote-SSH /
  Remote-Tunnels / Dev Containers, AWS Cloud9 (and its deprecation)?
- What user expectations have these set (startup time, persistence,
  reconnect, cost transparency, "it just feels local")? What recurring
  complaints show up in reviews, issues, and forums?

**2. Ephemeral dev pods on Kubernetes / EKS — patterns & best practices**
- Pod vs. Deployment vs. StatefulSet vs. Job for a single-developer
  workspace; one-pod-per-developer ("VDI-style") patterns.
- Scale-to-zero / idle shutdown and warm-pool/standby approaches; how
  teams balance cold-start latency against idle cost.
- Right-sizing (requests/limits), `kubectl exec` reliability for
  long-lived sessions, TTY/resize handling, exec timeouts and proxy/idle
  disconnects, and how people keep `exec`-based sessions alive.
- Node provisioning realities: Karpenter vs. Cluster Autoscaler vs.
  managed node groups vs. **Fargate** (note Fargate limitations relevant
  to dev: no privileged, no DaemonSets, storage/`exec` constraints).

**3. Storage for code in the pod (the durability story)**
- **Mountpoint for Amazon S3 / S3 CSI driver**: documented limitations
  that bite a code editor specifically — no `rename`, no random/in-place
  writes, append behavior, write-on-close semantics, consistency,
  performance for many small files, metadata/`stat` cost. How do tools
  work around the no-rename limitation for "atomic save"?
- Alternatives and their trade-offs for a workspace: **EFS** (latency,
  cost, small-file performance), **EBS** (single-AZ, RWO, attach/detach
  on reschedule), `s3fs`/`goofys`, and "local disk + sync to S3"
  approaches. When is each chosen?
- How do real systems get "durable when the pod is gone, fast while
  it's running"? Patterns for snapshot/restore, sync daemons, overlay/
  cache layers, git-based persistence.

**4. Bring-your-own-cluster / bring-your-own-AWS auth & access**
- EKS authentication for tools and humans: `aws eks get-token`,
  `aws-iam-authenticator`, the kubeconfig `exec` credential flow, **EKS
  access entries vs. the legacy `aws-auth` ConfigMap**, SSO/IAM Identity
  Center.
- In-pod AWS access: **IRSA vs. EKS Pod Identity** — current guidance,
  trade-offs, common misconfigurations (so the pod can read its S3
  bucket without baked-in keys).
- Least-privilege IAM for "let a developer spin up and exec into a dev
  pod, scoped to a namespace/bucket." Common over-permissioning
  mistakes. Multi-account / multi-cluster context-switching pain.

**5. Provisioning flows teams actually use (the customization surface)**
- How teams manage per-developer dev environments with **Terraform**
  (and Pulumi/CDK/Helm/Kustomize): repo layout, per-user workspaces/
  state, `terraform output` as an integration point, apply latency,
  drift, who runs apply (developer vs. platform vs. CI), and how an
  external tool should *invoke* these without owning them.
- Self-service models (Backstage, internal CLIs, `make` targets) and
  what makes them feel good vs. painful.
- For an editor that shells out to user-provided up/down/status commands
  and parses their output: what output contract is robust (JSON markers,
  exit codes, streaming logs)? What goes wrong in practice?

**6. The attach/UX layer**
- What makes "connect to my cloud workspace" feel fast and trustworthy:
  progress/log streaming, reconnect after network drop or pod reschedule
  (pod name changes!), clear status, graceful teardown.
- Failure modes that frustrate users: silent half-attach, stale
  contexts, expired tokens mid-session, exec disconnects, lost unsaved
  work, surprise bills from forgotten pods.

**7. Cost, governance, and pitfalls**
- Concrete cost-control patterns: idle auto-stop, TTLs, budgets/alerts,
  spot for dev pods, scale-to-zero, quota guardrails.
- Security pitfalls specific to dev pods (privileged containers, host
  mounts, broad `exec` rights, secrets in env, S3 bucket scoping).
- The top "I wish I'd known" lessons and anti-patterns from teams who
  built internal cloud-dev platforms.

### Output format

1. **Executive summary** (≤1 page): the 8-12 highest-signal findings,
   each one line, each actionable for our design.
2. **Per-section findings** (sections 1-7 above): bullets, each with a
   cited source + date and a one-line "implication for us."
3. **Comparison tables** where useful: (a) cloud-dev products — model,
   persistence, BYO-cloud support, top complaint; (b) pod storage
   options — durability, speed, `rename` support, cost, when to choose.
4. **Pitfalls & anti-patterns**: ranked list, each with the failure it
   causes and the mitigation.
5. **Open questions / where sources conflict or are thin.**
6. **Direct recommendations** for: the pod storage choice + how to
   handle "atomic save" against S3 mounts; the EKS auth approach to
   assume; the provisioning-command contract for the customization escape
   hatch; the default cost guardrails; and the reconnect-after-reschedule
   strategy.

Be concrete and skeptical. Prefer specifics (flags, limits, version
caveats, real numbers) over generalities. If something is commonly
believed but wrong or outdated, say so and cite why.
