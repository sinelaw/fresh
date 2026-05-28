/// <reference path="./lib/fresh.d.ts" />

/**
 * Environment Manager
 *
 * Detects a project's environment manager (Python venv, direnv, mise) and
 * activates it by handing core an activation **snippet** via `editor.setEnv`.
 * Core captures the resulting environment on the active backend (local / SSH)
 * and applies it to every editor-spawned process — language servers,
 * formatters, `spawnProcess`.
 *
 * Detection is passive (reads files only). Activation runs repo-controlled
 * code, so it is gated on Workspace Trust: the plugin only calls `setEnv` when
 * `editor.workspaceTrustLevel() === "trusted"` (and core enforces the same).
 *
 * Freshness: one-shot spawns re-capture automatically when the env inputs
 * change (core's cache is keyed on them). A long-running language server has
 * its env fixed at spawn, so to pick up a changed `.envrc`/`mise.toml` the
 * user runs **Env: Reload**, which re-captures and restarts servers. (Auto
 * file-watching is intentionally not wired yet.)
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
 * snippet, or null if none. These are auto-detected default snippets; direnv
 * and mise need their exporters (they're prompt-hook driven), venv sources its
 * activate script, and anything else is a pure login shell / user snippet.
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

/** Activate (or, when already active, reload) the detected environment. */
function activate(): void {
  if (!isTrusted()) {
    editor.setStatus(editor.t("status.not_trusted"));
    return;
  }
  const det = detect();
  if (!det) {
    editor.setStatus(editor.t("status.no_env_detected"));
    return;
  }
  // Core captures `snippet` on the active backend and applies it to every
  // spawn; it restarts so language servers re-spawn under the fresh env.
  editor.setEnv(det.snippet, editor.getCwd());
  editor.setStatus(
    editor.t(editor.envActive() ? "status.reloading" : "status.activating", { name: det.name }),
  );
}
registerHandler("env_activate_handler", activate);

function useSystem(): void {
  editor.clearEnv();
  editor.setStatus(editor.t("status.deactivated"));
}
registerHandler("env_use_system_handler", useSystem);

function showStatus(): void {
  const det = detect();
  const trust = editor.workspaceTrustLevel() || "unavailable";
  if (editor.envActive()) {
    editor.setStatus(
      det
        ? editor.t("status.env_active_named", { name: det.name })
        : editor.t("status.env_active"),
    );
  } else if (det) {
    editor.setStatus(editor.t("status.env_detected", { name: det.name, trust }));
  } else {
    editor.setStatus(editor.t("status.no_env", { trust }));
  }
}
registerHandler("env_status_handler", showStatus);

editor.registerCommand("%cmd.activate", "%cmd.activate_desc", "env_activate_handler");
editor.registerCommand("%cmd.reload", "%cmd.reload_desc", "env_activate_handler");
editor.registerCommand("%cmd.use_system", "%cmd.use_system_desc", "env_use_system_handler");
editor.registerCommand("%cmd.status", "%cmd.status_desc", "env_status_handler");

// === Status pill (opt-in to a user's status-bar layout) ===

function refreshStatus(): void {
  const bufferId = editor.getActiveBufferId();
  if (bufferId === 0) return;
  const det = detect();
  let value: string;
  if (editor.envActive()) {
    value = det
      ? editor.t("statusbar.active", { name: det.name })
      : editor.t("statusbar.active_unknown");
  } else if (det) {
    value = isTrusted()
      ? det.name
      : editor.t("statusbar.locked", { name: det.name });
  } else {
    value = editor.t("statusbar.system");
  }
  editor.setStatusBarValue(bufferId, STATUS_TOKEN, value);
}

editor.registerStatusBarElement(STATUS_TOKEN, editor.t("statusbar.label"));

registerHandler("env_refresh_status", refreshStatus);
for (const event of ["buffer_activated", "after_file_open", "focus_gained"]) {
  editor.on(event, "env_refresh_status");
}

refreshStatus();
