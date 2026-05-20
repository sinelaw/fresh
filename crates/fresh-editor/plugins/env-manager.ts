/// <reference path="./lib/fresh.d.ts" />

/**
 * Environment Manager
 *
 * Detects a project's environment manager (Python venv, direnv, mise) and
 * "activates" it by injecting its environment into every editor-spawned
 * process — language servers, formatters, `spawnProcess`, the terminal — so
 * they see the same `PATH`/`VIRTUAL_ENV` the user's shell would.
 *
 * Security: activation runs repo-controlled code (`.envrc`, repo-local
 * interpreters on `PATH`), so it is gated on Workspace Trust. The plugin
 * never activates unless `editor.workspaceTrustLevel() === "trusted"`.
 *
 * Mechanism: it captures the environment and installs it via
 * `editor.setAuthority({ spawner: { kind: "local-with-env", env } })`, which
 * the core applies to the local spawner. setAuthority restarts the editor;
 * the reloaded plugin instance recognizes the active environment from the
 * authority label (`env: …`).
 */

const editor = getEditor();

const STATUS_TOKEN = "env";

type ProviderKind = "venv" | "direnv" | "mise";

interface Detected {
  kind: ProviderKind;
  /** Absolute path of the venv directory (venv only). */
  dir?: string;
  /** Short label, e.g. ".venv" / "direnv" / "mise". */
  name: string;
}

// === Detection (passive — reads files only, never executes) ===

function fileExists(p: string): boolean {
  try {
    return editor.fileExists(p);
  } catch (_e) {
    return false;
  }
}

/** Detect the environment in the current workspace, or null if none. */
function detect(): Detected | null {
  const cwd = editor.getCwd();
  if (!cwd) return null;

  // Python virtual environment: prefer `.venv`, then `venv`.
  for (const name of [".venv", "venv"]) {
    const dir = editor.pathJoin(cwd, name);
    if (
      fileExists(editor.pathJoin(dir, "bin", "python")) ||
      fileExists(editor.pathJoin(dir, "bin", "python3")) ||
      fileExists(editor.pathJoin(dir, "Scripts", "python.exe"))
    ) {
      return { kind: "venv", dir, name };
    }
  }

  if (fileExists(editor.pathJoin(cwd, ".envrc"))) {
    return { kind: "direnv", name: "direnv" };
  }

  for (const name of ["mise.toml", ".mise.toml", ".tool-versions"]) {
    if (fileExists(editor.pathJoin(cwd, name))) {
      return { kind: "mise", name: "mise" };
    }
  }

  return null;
}

// === State helpers ===

/** Whether an environment is currently active (we own the authority). */
function isActive(): boolean {
  return editor.getAuthorityLabel().startsWith("env:");
}

function isTrusted(): boolean {
  return editor.workspaceTrustLevel() === "trusted";
}

// === Environment capture (per provider) ===

/** Parse a `{ KEY: value | null }` JSON object into env pairs (skip nulls). */
function jsonToEnvPairs(stdout: string): [string, string][] {
  const out: [string, string][] = [];
  let obj: unknown;
  try {
    obj = JSON.parse(stdout || "{}");
  } catch (_e) {
    return out;
  }
  if (obj && typeof obj === "object" && !Array.isArray(obj)) {
    for (const [k, v] of Object.entries(obj as Record<string, unknown>)) {
      if (typeof v === "string") out.push([k, v]);
    }
  }
  return out;
}

/**
 * Capture the environment for `det`. Returns the env pairs to inject, or
 * null on failure (a status message is set). May run a subprocess (direnv /
 * mise), which is why callers must check trust first.
 */
async function captureEnv(det: Detected): Promise<[string, string][] | null> {
  const cwd = editor.getCwd();

  if (det.kind === "venv") {
    const dir = det.dir!;
    const binDir = editor.pathJoin(dir, "bin");
    const oldPath = editor.getEnv("PATH") ?? "";
    const sep = oldPath.length > 0 ? ":" : "";
    return [
      ["VIRTUAL_ENV", dir],
      ["PATH", `${binDir}${sep}${oldPath}`],
    ];
  }

  if (det.kind === "direnv") {
    const r = await editor.spawnProcess("direnv", ["export", "json"], cwd);
    if (r.exit_code !== 0) {
      // direnv prints "is blocked" guidance to stderr until `direnv allow`.
      editor.setStatus(
        `direnv export failed (exit ${r.exit_code}). Run \`direnv allow\` in this project.`,
      );
      return null;
    }
    const pairs = jsonToEnvPairs(r.stdout);
    if (pairs.length === 0) {
      editor.setStatus("direnv produced no environment (is .envrc empty / allowed?)");
      return null;
    }
    return pairs;
  }

  // mise
  const r = await editor.spawnProcess("mise", ["env", "--json"], cwd);
  if (r.exit_code !== 0) {
    editor.setStatus(`mise env failed (exit ${r.exit_code}): ${r.stderr.trim()}`);
    return null;
  }
  return jsonToEnvPairs(r.stdout);
}

// === Activation / deactivation ===

async function activate(): Promise<void> {
  if (!isTrusted()) {
    editor.setStatus(
      "Workspace not trusted — run “Workspace Trust: Trust This Folder” to activate the environment",
    );
    return;
  }
  const det = detect();
  if (!det) {
    editor.setStatus("No environment manager detected in this project");
    return;
  }

  const env = await captureEnv(det);
  if (env === null) return; // captureEnv set a status

  const label = `env: ${det.name}`;
  // setAuthority restarts the editor; nothing after this runs.
  editor.setAuthority({
    filesystem: { kind: "local" },
    spawner: { kind: "local-with-env", env },
    terminal_wrapper: { kind: "host-shell" },
    display_label: label,
  });
}

function useSystem(): void {
  if (!isActive()) {
    editor.setStatus("No environment is active");
    return;
  }
  // Restore the plain local authority (no injected env). Restarts the editor.
  editor.setAuthority({
    filesystem: { kind: "local" },
    spawner: { kind: "local" },
    terminal_wrapper: { kind: "host-shell" },
    display_label: "",
  });
}

// === Status surface ===

function statusValue(): string {
  if (isActive()) return editor.getAuthorityLabel();
  const det = detect();
  if (!det) return "system";
  return isTrusted() ? `${det.name} (inactive)` : `${det.name} (locked)`;
}

function refreshStatus(): void {
  const bufferId = editor.getActiveBufferId();
  if (bufferId === 0) return;
  editor.setStatusBarValue(bufferId, STATUS_TOKEN, statusValue());
}

// === Commands ===

function env_activate_handler(): void {
  void activate();
}
registerHandler("env_activate_handler", env_activate_handler);

function env_use_system_handler(): void {
  useSystem();
}
registerHandler("env_use_system_handler", env_use_system_handler);

function env_status_handler(): void {
  const det = detect();
  const active = isActive();
  const trust = editor.workspaceTrustLevel() || "unavailable";
  if (active) {
    editor.setStatus(`Active: ${editor.getAuthorityLabel()} (trust: ${trust})`);
  } else if (det) {
    editor.setStatus(
      `Detected ${det.name}; not active (trust: ${trust}). Run “Env: Activate”.`,
    );
  } else {
    editor.setStatus(`No environment detected (trust: ${trust})`);
  }
}
registerHandler("env_status_handler", env_status_handler);

editor.registerCommand(
  "env_activate",
  "Env: Activate Detected Environment (venv / direnv / mise)",
  "env_activate_handler",
);
editor.registerCommand(
  "env_use_system",
  "Env: Use System (Deactivate Environment)",
  "env_use_system_handler",
);
editor.registerCommand(
  "env_status",
  "Env: Show Environment Status",
  "env_status_handler",
);

// === Wiring ===

editor.registerStatusBarElement(STATUS_TOKEN, "Environment");

registerHandler("env_refresh_status", refreshStatus);
for (const event of ["buffer_activated", "after_file_open", "focus_gained"]) {
  editor.on(event, "env_refresh_status");
}

// Populate the status value at load (covers the post-activation restart too).
refreshStatus();
