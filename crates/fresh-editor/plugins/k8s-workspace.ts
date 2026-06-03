/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Kubernetes Workspace Plugin
 *
 * Bring-your-own-cluster management for editing inside
 * an ephemeral Kubernetes pod. Everything customizable funnels through one
 * small `Provider` contract: given a workspace target, produce the pod
 * coordinates to attach to (and reverse it on disconnect).
 *
 *   attach-existing  pick an already-running pod (zero infra)
 *   manifest         kubectl apply a (templated) Pod/Job manifest, wait Ready
 *   run              kubectl run a throwaway pod, wait Ready
 *   command          run ANY user command and read PodCoords JSON back —
 *                    the escape hatch for Terraform/Helm/CDK/internal CLIs
 *
 * The plugin only ever does host-side work via `editor.spawnHostProcess`
 * (which inherits the user's kubeconfig + cloud credential chain) and hands
 * the resolved pod to core via `editor.attachRemoteAgent(...)`. Core owns
 * the agent bootstrap + the editor restart; the plugin owns the lifecycle.
 *
 * Config is layered: a repo-local `.fresh/k8s.json` (shareable team
 * targets) plus a zero-config fallback that prompts for namespace/pod.
 *
 * See docs/internal/K8S_WORKSPACE_PLUGIN_DESIGN.md.
 */

// =============================================================================
// Types
// =============================================================================

interface PodCoords {
  context?: string;
  namespace: string;
  pod: string;
  container?: string;
  /** Pod-side path of the mounted workspace (terminal cwd). */
  workspace?: string;
}

interface CommandSpec {
  command: string;
  args?: string[];
  cwd?: string;
}

type ProviderSpec =
  | { kind: "attach-existing"; context?: string; namespace?: string }
  | {
      kind: "manifest";
      template: string;
      namespace: string;
      context?: string;
      waitTimeoutSec?: number;
    }
  | {
      kind: "run";
      name: string;
      image: string;
      namespace: string;
      context?: string;
      waitTimeoutSec?: number;
    }
  | {
      kind: "command";
      up: CommandSpec;
      down?: CommandSpec;
      status?: CommandSpec;
    };

interface TargetConfig {
  provider: ProviderSpec;
  /** Template variables substituted into manifests / command args. */
  vars?: Record<string, string>;
  /** Default cwd inside the pod (the mounted workspace). */
  workspace?: string;
  /** Ask before creating a pod (default true). */
  confirmCreate?: boolean;
}

interface K8sConfig {
  defaultTarget?: string;
  targets: Record<string, TargetConfig>;
}

/** What the plugin remembers about the live session for disconnect. */
interface ActiveSession {
  targetName: string;
  coords: PodCoords;
}

// =============================================================================
// Config
// =============================================================================

function configPath(): string {
  return editor.pathJoin(editor.getCwd(), ".fresh", "k8s.json");
}

function loadConfig(): K8sConfig | null {
  const path = configPath();
  if (!editor.fileExists(path)) return null;
  const text = editor.readFile(path);
  if (text === null) return null;
  try {
    const parsed = editor.parseJsonc(text) as K8sConfig;
    if (!parsed || typeof parsed !== "object" || !parsed.targets) {
      editor.setStatus("K8s: .fresh/k8s.json has no `targets`");
      return null;
    }
    return parsed;
  } catch (e) {
    editor.setStatus(`K8s: failed to parse .fresh/k8s.json: ${errMsg(e)}`);
    return null;
  }
}

function errMsg(e: unknown): string {
  return e instanceof Error ? e.message : String(e);
}

/** Substitute ${var} / ${user} / ${workspace} tokens. */
function expand(s: string, vars: Record<string, string>): string {
  return s.replace(/\$\{(\w+)\}/g, (_m, k: string) =>
    k in vars ? vars[k] : `\${${k}}`,
  );
}

function expandArgs(args: string[] | undefined, vars: Record<string, string>): string[] {
  return (args ?? []).map((a) => expand(a, vars));
}

// =============================================================================
// kubectl helpers
// =============================================================================

/** Common pod-scoping flags for kubectl, shared by exec/get/wait. */
function scopeArgs(context: string | undefined, namespace: string): string[] {
  const a: string[] = [];
  if (context) a.push("--context", context);
  a.push("-n", namespace);
  return a;
}

async function kubectl(args: string[]): Promise<SpawnResult> {
  return await editor.spawnHostProcess("kubectl", args);
}

/** True if the developer identity can `create pods/exec` in the namespace —
 *  the K8s >=1.30/1.35 WebSocket-exec RBAC requirement that otherwise fails
 *  attach with a confusing error after a cluster upgrade. */
async function canExec(context: string | undefined, namespace: string): Promise<boolean> {
  const res = await kubectl([
    ...scopeArgs(context, namespace),
    "auth",
    "can-i",
    "create",
    "pods/exec",
  ]);
  return res.exit_code === 0 && res.stdout.trim() === "yes";
}

/** Capture the in-pod login env so LSP binaries on a shell-only PATH
 *  (e.g. ~/.local/bin) resolve when core goes to spawn them. */
async function probeEnv(coords: PodCoords): Promise<[string, string][]> {
  const args = [...scopeArgs(coords.context, coords.namespace), "exec"];
  if (coords.container) args.push("-c", coords.container);
  args.push(coords.pod, "--", "sh", "-lc", "env");
  const res = await kubectl(args);
  if (res.exit_code !== 0) return [];
  const out: [string, string][] = [];
  for (const line of res.stdout.split("\n")) {
    const eq = line.indexOf("=");
    if (eq <= 0) continue;
    const key = line.slice(0, eq);
    if (!/^[A-Za-z_][A-Za-z0-9_]*$/.test(key)) continue;
    out.push([key, line.slice(eq + 1)]);
  }
  return out;
}

async function waitReady(
  context: string | undefined,
  namespace: string,
  pod: string,
  timeoutSec: number,
): Promise<boolean> {
  const res = await kubectl([
    ...scopeArgs(context, namespace),
    "wait",
    "--for=condition=Ready",
    `pod/${pod}`,
    `--timeout=${timeoutSec}s`,
  ]);
  return res.exit_code === 0;
}

// =============================================================================
// Providers — resolve a target to PodCoords
// =============================================================================

/** Parse the last non-empty stdout line as PodCoords JSON (the `command`
 *  provider contract). Tolerates trailing log noise on earlier lines. */
function parsePodCoords(stdout: string): PodCoords | null {
  const lines = stdout
    .split("\n")
    .map((l) => l.trim())
    .filter((l) => l.length > 0);
  for (let i = lines.length - 1; i >= 0; i--) {
    try {
      const obj = JSON.parse(lines[i]) as PodCoords;
      if (obj && typeof obj.namespace === "string" && typeof obj.pod === "string") {
        return obj;
      }
    } catch {
      // not JSON; keep scanning upward
    }
  }
  return null;
}

async function resolveProvider(
  name: string,
  target: TargetConfig,
): Promise<PodCoords | null> {
  const vars: Record<string, string> = {
    user: editor.getCwd().split("/").pop() ?? "dev",
    workspace: target.workspace ?? "/workspace",
    cwd: editor.getCwd(),
    ...(target.vars ?? {}),
  };
  const provider = target.provider;

  switch (provider.kind) {
    case "attach-existing":
      return await attachExisting(provider.context, provider.namespace, target.workspace);

    case "command": {
      editor.setStatus(`K8s: provisioning '${name}'…`);
      const res = await editor.spawnHostProcess(
        provider.up.command,
        expandArgs(provider.up.args, vars),
        provider.up.cwd ? expand(provider.up.cwd, vars) : undefined,
      );
      if (res.exit_code !== 0) {
        editor.setStatus(`K8s: provisioning failed: ${firstLine(res.stderr || res.stdout)}`);
        return null;
      }
      const coords = parsePodCoords(res.stdout);
      if (!coords) {
        editor.setStatus("K8s: provisioning command emitted no PodCoords JSON");
        return null;
      }
      return withWorkspace(coords, target.workspace);
    }

    case "manifest": {
      if (!(await confirmCreate(target, name))) return null;
      const text = editor.readFile(expand(provider.template, vars));
      if (text === null) {
        editor.setStatus(`K8s: manifest not found: ${provider.template}`);
        return null;
      }
      // Apply the rendered manifest via stdin is not available, so write a
      // temp file? Keep it robust: apply the file path directly after the
      // template has been rendered by the user's own tooling. For ${var}
      // templating we apply through `kubectl apply -f -` is unavailable;
      // require a concrete file the user keeps rendered, or rely on `run`.
      const applied = await kubectl([
        ...scopeArgs(provider.context, provider.namespace),
        "apply",
        "-f",
        expand(provider.template, vars),
      ]);
      if (applied.exit_code !== 0) {
        editor.setStatus(`K8s: kubectl apply failed: ${firstLine(applied.stderr)}`);
        return null;
      }
      const pod = await promptPod(provider.context, provider.namespace);
      if (!pod) return null;
      const ok = await waitReady(
        provider.context,
        provider.namespace,
        pod,
        provider.waitTimeoutSec ?? 180,
      );
      if (!ok) {
        editor.setStatus(`K8s: pod ${pod} did not become Ready`);
        return null;
      }
      return withWorkspace(
        { context: provider.context, namespace: provider.namespace, pod },
        target.workspace,
      );
    }

    case "run": {
      if (!(await confirmCreate(target, name))) return null;
      const podName = expand(provider.name, vars);
      const created = await kubectl([
        ...scopeArgs(provider.context, provider.namespace),
        "run",
        podName,
        `--image=${expand(provider.image, vars)}`,
        "--restart=Never",
        "--command",
        "--",
        "sh",
        "-c",
        "trap : TERM INT; sleep infinity & wait",
      ]);
      if (created.exit_code !== 0) {
        editor.setStatus(`K8s: kubectl run failed: ${firstLine(created.stderr)}`);
        return null;
      }
      const ok = await waitReady(
        provider.context,
        provider.namespace,
        podName,
        provider.waitTimeoutSec ?? 180,
      );
      if (!ok) {
        editor.setStatus(`K8s: pod ${podName} did not become Ready`);
        return null;
      }
      return withWorkspace(
        { context: provider.context, namespace: provider.namespace, pod: podName },
        target.workspace,
      );
    }
  }
}

function withWorkspace(coords: PodCoords, workspace?: string): PodCoords {
  if (coords.workspace) return coords;
  return { ...coords, workspace: workspace ?? "/workspace" };
}

function firstLine(s: string): string {
  return s.split("\n").find((l) => l.trim().length > 0) ?? s;
}

async function confirmCreate(target: TargetConfig, name: string): Promise<boolean> {
  if (target.confirmCreate === false) return true;
  const answer = await editor.prompt(
    `Create a cloud workspace pod for '${name}'? This may create a pod (and incur cost on managed clusters). (y/N)`,
    "",
  );
  return answer !== null && /^y(es)?$/i.test(answer.trim());
}

// =============================================================================
// attach-existing (zero-config fallback)
// =============================================================================

async function attachExisting(
  context: string | undefined,
  namespace: string | undefined,
  workspace: string | undefined,
): Promise<PodCoords | null> {
  let ns = namespace;
  if (!ns) {
    const entered = await editor.prompt("Kubernetes namespace:", "default");
    if (entered === null || entered.trim() === "") return null;
    ns = entered.trim();
  }
  const pod = await promptPod(context, ns);
  if (!pod) return null;
  return withWorkspace({ context, namespace: ns, pod }, workspace);
}

/** List Running pods in the namespace and let the user pick one by name. */
async function promptPod(
  context: string | undefined,
  namespace: string,
): Promise<string | null> {
  const res = await kubectl([
    ...scopeArgs(context, namespace),
    "get",
    "pods",
    "--field-selector=status.phase=Running",
    "-o",
    "name",
  ]);
  if (res.exit_code !== 0) {
    editor.setStatus(`K8s: kubectl get pods failed: ${firstLine(res.stderr)}`);
    return null;
  }
  const pods = res.stdout
    .split("\n")
    .map((l) => l.trim().replace(/^pod\//, ""))
    .filter((l) => l.length > 0);
  if (pods.length === 0) {
    editor.setStatus(`K8s: no Running pods in namespace ${namespace}`);
    return null;
  }
  const hint = pods.length === 1 ? pods[0] : pods.slice(0, 5).join(", ");
  const chosen = await editor.prompt(`Pod to attach (${hint}):`, pods[0]);
  if (chosen === null || chosen.trim() === "") return null;
  const name = chosen.trim();
  if (!pods.includes(name)) {
    editor.setStatus(`K8s: pod '${name}' is not a Running pod in ${namespace}`);
    return null;
  }
  return name;
}

// =============================================================================
// Connect / Disconnect
// =============================================================================

const SESSION_KEY = "k8s.activeSession";

async function chooseTarget(config: K8sConfig): Promise<[string, TargetConfig] | null> {
  const names = Object.keys(config.targets);
  if (names.length === 0) return null;
  if (names.length === 1) return [names[0], config.targets[names[0]]];
  const def = config.defaultTarget && names.includes(config.defaultTarget)
    ? config.defaultTarget
    : names[0];
  const chosen = await editor.prompt(`Target (${names.join(", ")}):`, def);
  if (chosen === null) return null;
  const name = chosen.trim();
  const target = config.targets[name];
  if (!target) {
    editor.setStatus(`K8s: unknown target '${name}'`);
    return null;
  }
  return [name, target];
}

async function connectWorkspace(): Promise<void> {
  const config = loadConfig();

  let name: string;
  let target: TargetConfig;
  if (config) {
    const picked = await chooseTarget(config);
    if (!picked) return;
    [name, target] = picked;
  } else {
    // Zero-config: attach to an existing pod chosen by prompt.
    name = "adhoc";
    target = { provider: { kind: "attach-existing" } };
  }

  const coords = await resolveProvider(name, target);
  if (!coords) return;

  // Preflight: python3 present (agent prereq) is checked implicitly by the
  // agent bootstrap; here we catch the RBAC gotcha early with a clear error.
  if (!(await canExec(coords.context, coords.namespace))) {
    editor.setStatus(
      `K8s: missing 'create pods/exec' on ${coords.namespace} — exec/attach will be denied (K8s WebSocket-exec RBAC)`,
    );
    return;
  }

  editor.setStatus(`K8s: probing env in ${coords.pod}…`);
  const baseEnv = await probeEnv(coords);

  const spec: RemoteAgentSpec = {
    transport: {
      kind: "kubectl-exec",
      context: coords.context ?? null,
      namespace: coords.namespace,
      pod: coords.pod,
      container: coords.container ?? null,
      workspace: coords.workspace ?? null,
    },
    base_env: baseEnv,
  };

  // Remember the session so Disconnect can run the provider's teardown.
  const session: ActiveSession = { targetName: name, coords };
  editor.setGlobalState(SESSION_KEY, session as unknown);

  editor.setStatus(`K8s: attaching to ${coords.namespace}/${coords.pod}…`);
  // Fire-and-forget: core connects asynchronously and restarts on success.
  editor.attachRemoteAgent(spec);
}

async function disconnectWorkspace(): Promise<void> {
  const raw = editor.getGlobalState(SESSION_KEY) as ActiveSession | null;

  // Detach the editor first (restores local authority via restart).
  editor.clearAuthority();

  if (raw && raw.coords) {
    const config = loadConfig();
    const target = config?.targets[raw.targetName];
    const provider = target?.provider;
    if (provider && provider.kind === "command" && provider.down) {
      const vars: Record<string, string> = {
        user: editor.getCwd().split("/").pop() ?? "dev",
        workspace: raw.coords.workspace ?? "/workspace",
        cwd: editor.getCwd(),
        ...(target?.vars ?? {}),
      };
      editor.setStatus("K8s: tearing down workspace…");
      await editor.spawnHostProcess(
        provider.down.command,
        expandArgs(provider.down.args, vars),
        provider.down.cwd ? expand(provider.down.cwd, vars) : undefined,
      );
    }
  }
  editor.setGlobalState(SESSION_KEY, null);
}

// =============================================================================
// Registration
// =============================================================================

function k8s_connect(): void {
  connectWorkspace().catch((e) => editor.setStatus(`K8s: ${errMsg(e)}`));
}

function k8s_disconnect(): void {
  disconnectWorkspace().catch((e) => editor.setStatus(`K8s: ${errMsg(e)}`));
}

registerHandler("k8s_connect", k8s_connect);
registerHandler("k8s_disconnect", k8s_disconnect);

editor.registerCommand(
  "K8s: Connect Workspace",
  "Connect / attach to a Kubernetes pod workspace via kubectl exec — edit inside a remote pod on any cluster: EKS, GKE, AKS, k3d, minikube, kind. Keywords: kubernetes k8s kubectl pod container cluster remote cloud workspace attach.",
  "k8s_connect",
);
editor.registerCommand(
  "K8s: Disconnect Workspace",
  "Disconnect / detach from the Kubernetes pod workspace and restore local editing. Keywords: kubernetes k8s kubectl pod cluster remote cloud disconnect detach.",
  "k8s_disconnect",
);
