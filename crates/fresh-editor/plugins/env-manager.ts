/// <reference path="./lib/fresh.d.ts" />

/**
 * Environment Manager
 *
 * Detects a project's environment manager (Python venv, direnv, mise) and
 * activates it by handing core an activation **snippet** via `editor.setEnv`.
 * Core captures the resulting environment on the active backend (local / SSH /
 * docker) and applies it to every editor-spawned process — language servers,
 * formatters, `spawnProcess`, the terminal.
 *
 * Detection is passive (reads files only). Activation runs repo-controlled
 * code, so it is gated on Workspace Trust: the plugin only calls `setEnv` when
 * `editor.workspaceTrustLevel() === "trusted"` (and core enforces the same).
 *
 * This plugin holds no env vars and does no capture itself — it just picks the
 * right snippet. The snippet is the universal recipe; the named managers below
 * are auto-detected defaults, all overridable by the user.
 */

const editor = getEditor();

const STATUS_TOKEN = "env";

interface Detected {
  /** Short label for the status pill, e.g. ".venv" / "direnv" / "mise". */
  name: string;
  /** The activation snippet handed to `editor.setEnv`. */
  snippet: string;
}

function fileExists(p: string): boolean {
  try {
    return editor.fileExists(p);
  } catch (_e) {
    return false;
  }
}

/**
 * Detect the environment in the current workspace and return its activation
 * snippet, or null if none. These are the auto-detected default snippets;
 * direnv/mise need their exporters (they're prompt-hook driven), venv sources
 * its activate script, and anything else is a pure login shell / user snippet.
 */
function detect(): Detected | null {
  const cwd = editor.getCwd();
  if (!cwd) return null;

  for (const name of [".venv", "venv"]) {
    const dir = editor.pathJoin(cwd, name);
    if (
      fileExists(editor.pathJoin(dir, "bin", "python")) ||
      fileExists(editor.pathJoin(dir, "bin", "python3")) ||
      fileExists(editor.pathJoin(dir, "Scripts", "python.exe"))
    ) {
      return { name, snippet: `source ${editor.pathJoin(dir, "bin", "activate")}` };
    }
  }

  if (fileExists(editor.pathJoin(cwd, ".envrc"))) {
    return { name: "direnv", snippet: `eval "$(direnv export bash)"` };
  }

  for (const name of ["mise.toml", ".mise.toml", ".tool-versions"]) {
    if (fileExists(editor.pathJoin(cwd, name))) {
      return { name: "mise", snippet: `eval "$(mise env -s bash)"` };
    }
  }

  return null;
}

function isTrusted(): boolean {
  return editor.workspaceTrustLevel() === "trusted";
}

// === Commands ===

function activate(): void {
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
  // Core captures `snippet` on the active backend and applies it to every
  // spawn; it restarts the editor so already-running tooling picks it up.
  editor.setEnv(det.snippet, editor.getCwd());
  editor.setStatus(`Activating ${det.name} environment…`);
}
registerHandler("env_activate_handler", activate);

function useSystem(): void {
  editor.clearEnv();
  editor.setStatus("Environment deactivated — using the system environment");
}
registerHandler("env_use_system_handler", useSystem);

function showStatus(): void {
  const det = detect();
  const trust = editor.workspaceTrustLevel() || "unavailable";
  if (det) {
    editor.setStatus(
      `Detected ${det.name} (trust: ${trust}). Run “Env: Activate” to use it.`,
    );
  } else {
    editor.setStatus(`No environment detected (trust: ${trust})`);
  }
}
registerHandler("env_status_handler", showStatus);

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

// === Status pill (opt-in to a user's status-bar layout) ===

function refreshStatus(): void {
  const bufferId = editor.getActiveBufferId();
  if (bufferId === 0) return;
  const det = detect();
  const value = det
    ? `${det.name}${isTrusted() ? "" : " (locked)"}`
    : "system";
  editor.setStatusBarValue(bufferId, STATUS_TOKEN, value);
}

editor.registerStatusBarElement(STATUS_TOKEN, "Environment");

registerHandler("env_refresh_status", refreshStatus);
for (const event of ["buffer_activated", "after_file_open", "focus_gained"]) {
  editor.on(event, "env_refresh_status");
}

refreshStatus();
