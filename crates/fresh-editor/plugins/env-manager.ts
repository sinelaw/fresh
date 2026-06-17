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
 * ## Activation strategy (see `docs/internal/trust-env-devcontainer-ux-plan.md`)
 *
 * Detected envs are split by what their activation actually *does*:
 *
 * - **path-only** (`.venv` / `venv`): activation is a `PATH` prepend and a few
 *   env-var sets. No arbitrary shell is run against repo-controlled scripts
 *   *that the user authored* — yes, `activate` is sourced, but it's a fixed
 *   script that pyvenv/virtualenv writes. We treat this as low-risk and
 *   auto-activate on plugin load with no popup, mirroring VS Code Python.
 *   Undo is one click on the status pill (or `Env: Use System`).
 * - **shell** (`.envrc` / `mise.toml` / `.mise.toml` / `.tool-versions`):
 *   activation runs `direnv export` / `mise env`, which evaluate user shell
 *   inside the repo. This is the dangerous case, so it is gated on Workspace
 *   Trust. The plugin does *not* ask the trust question itself — the core
 *   workspace-trust modal is the single prompt for that, and it already names
 *   the specific `.envrc`/`mise` marker. When the user trusts the folder, core
 *   fires the `trust_changed` hook and this plugin activates the env in
 *   response. One decision, one prompt, no duplicate "Trust & activate" popup.
 *
 * Coordination with the devcontainer plugin: if a `devcontainer.json` is
 * present and the current authority is local, env-manager defers entirely —
 * the devcontainer plugin's "Reopen in Container?" popup goes first. After
 * the user attaches and the editor restarts under the container authority,
 * env-manager re-runs and asks about the env from inside the container, which
 * is the right place to do it. If the user dismisses the devcontainer popup,
 * env-manager picks up its own decision on the next plugin reload (next
 * editor restart).
 *
 * Freshness: one-shot spawns re-capture automatically when the env inputs
 * change (core's cache is keyed on them). A long-running language server has
 * its env fixed at spawn, so to pick up a changed `.envrc`/`mise.toml` the
 * user runs **Env: Reload**, which re-captures and restarts servers. (Auto
 * file-watching is intentionally not wired yet.)
 */

const editor = getEditor();

const STATUS_TOKEN = "env";

/// Devcontainer plugin's attach-popup id. We listen for its outcome on the
/// shared `action_popup_result` channel so we can un-defer the env popup
/// when the user declines the devcontainer attach — see the
/// `onDevcontainerAttachResult` handler below.
const DEVCONTAINER_ATTACH_POPUP_ID = "devcontainer-attach";

interface ActionPopupResultData {
  popup_id: string;
  action_id: string;
}

interface Detected {
  /** Short label for the status pill, e.g. ".venv" / "direnv" / "mise". */
  name: string;
  /** The activation snippet handed to `editor.setEnv`. */
  snippet: string;
  /**
   * "path-only" envs (`.venv`/`venv`) auto-activate silently when trusted.
   * "shell" envs (`.envrc`/`mise.toml`/`.tool-versions`) only activate once
   * the workspace is trusted (the core trust modal owns that prompt).
   */
  kind: "path-only" | "shell";
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
      return {
        name,
        snippet: `source ${editor.pathJoin(dir, "bin", "activate")}`,
        kind: "path-only",
      };
    }
  }

  if (fileExists(editor.pathJoin(cwd, ".envrc"))) {
    return {
      name: "direnv",
      snippet: `eval "$(direnv export bash)"`,
      kind: "shell",
    };
  }

  for (const name of ["mise.toml", ".mise.toml", ".tool-versions"]) {
    if (fileExists(editor.pathJoin(cwd, name))) {
      return {
        name: "mise",
        snippet: `eval "$(mise env -s bash)"`,
        kind: "shell",
      };
    }
  }

  return null;
}

function isTrusted(): boolean {
  return editor.workspaceTrustLevel() === "trusted";
}

/**
 * Whether a devcontainer config exists at the workspace root. Used to decide
 * whether to defer to the devcontainer plugin's attach popup. We do a passive
 * file check rather than reach across plugins so the two stay independent.
 */
function devcontainerConfigPresent(): boolean {
  const cwd = editor.getCwd();
  if (!cwd) return false;
  return (
    fileExists(editor.pathJoin(cwd, ".devcontainer", "devcontainer.json")) ||
    fileExists(editor.pathJoin(cwd, ".devcontainer.json"))
  );
}

/**
 * True when an authority other than "local" is installed — i.e. the editor
 * is already attached to a container or SSH host. Used by the defer-to-
 * devcontainer rule: we only stand aside *before* attach. After attach, the
 * authority is non-empty and we're free to surface our own popup.
 */
function authorityIsNonLocal(): boolean {
  return editor.getAuthorityLabel().length > 0;
}

// === Cross-plugin: devcontainer decline observation ===
//
// `maybeAutoActivate` defers when a `devcontainer.json` is present on the
// host so the devcontainer "Reopen in container?" popup goes first. The
// risk this introduces — and that this section closes — is that the
// devcontainer popup may *not actually appear* (the user previously
// chose "Ignore always", or already attached and detached, or some
// other skip-path in `devcontainer.ts:2729-2776`). Without a signal that
// devcontainer is *not* going to prompt, the env popup would never
// appear on the host, even though the user is staying local.
//
// Two pieces of state record what we've observed about devcontainer:
//
// - `devcontainerDismissedThisSession` (in-memory): set when the user
//   picks any non-attach option in the devcontainer-attach popup this
//   session. Re-running `maybeAutoActivate` after this flag is set
//   bypasses the defer guard and lets the env popup surface
//   immediately, in the same session.
// - `devcontainer-decline:<cwd>` in plugin global state (persisted):
//   set when the user picks "Ignore always" in the devcontainer popup.
//   On the *next* open of the same folder, devcontainer reads its own
//   persisted dismissal and silently skips the popup; without this
//   observation, env-manager would defer to a popup that never comes.
//   With it, env-manager proceeds straight to its own activate flow.
//
// We can't read devcontainer's own `attach:<cwd>` global state because
// plugin global state is namespaced per plugin (see fresh.d.ts:2700-2710).
// So we keep our own copy of the relevant observation, written when we
// see the user's choice on the shared `action_popup_result` channel.
let devcontainerDismissedThisSession = false;

function devcontainerObservationKey(): string {
  return "devcontainer-decline:" + editor.getCwd();
}

function readDevcontainerDeclined(): boolean {
  return editor.getGlobalState(devcontainerObservationKey()) === "user_dismissed";
}

function writeDevcontainerDeclined(): void {
  editor.setGlobalState(devcontainerObservationKey(), "user_dismissed");
}

// === Commands ===

/**
 * Apply `setEnv` and surface the activating/reloading status message.
 * Pre-condition: trust must already be Trusted (the caller is responsible).
 * Core captures the snippet on the active backend and restarts so language
 * servers re-spawn under the fresh env.
 */
function applyActivation(det: Detected): void {
  editor.setEnv(det.snippet, editor.getCwd());
  editor.setStatus(
    editor.t(editor.envActive() ? "status.reloading" : "status.activating", { name: det.name }),
  );
}

/** Activate (or, when already active, reload) the detected environment.
 *
 * Trust handling: if the workspace is not trusted, we open the *core*
 * workspace-trust modal (the single trust prompt) rather than a second,
 * plugin-owned trust popup. Once the user trusts the folder, core fires
 * `trust_changed` and our subscription below re-runs the activation — so the
 * user's one "Trust" decision both elevates trust and activates the env,
 * without us asking the trust question ourselves.
 */
function activate(): void {
  const det = detect();
  if (!det) {
    editor.setStatus(editor.t("status.no_env_detected"));
    return;
  }
  if (!isTrusted()) {
    requestCoreTrustPrompt();
    return;
  }
  applyActivation(det);
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

// === Trust prompt ===

/**
 * Open the *core* workspace-trust modal — the single, editor-owned trust
 * prompt. Plugins can't open it through a dedicated API, but the editor
 * exposes `workspace_trust_prompt` as an action and `executeActions` is the
 * generic dispatch channel. We deliberately do NOT show a second, plugin-owned
 * "Trust & activate" popup: that duplicated the trust question in a competing
 * UI. Activation happens as a *consequence* of trust, via the `trust_changed`
 * subscription below.
 */
function requestCoreTrustPrompt(): void {
  editor.executeActions([{ action: "workspace_trust_prompt", count: 1 }]);
}

/// Catch the devcontainer attach popup's outcome on the shared
/// `action_popup_result` channel. Any non-attach action means the user is
/// staying on the host, so we should un-defer and let the env popup
/// surface in the same session. `dismiss_always` is also persisted so
/// the next open of this folder doesn't re-defer (devcontainer will
/// silently skip its popup that time).
function onDevcontainerAttachResult(data: ActionPopupResultData): void {
  if (data.action_id === "attach") {
    // editor.setAuthority restarts the editor; env-manager re-runs
    // inside the container via the post-restart `plugins_loaded`.
    return;
  }
  devcontainerDismissedThisSession = true;
  if (data.action_id === "dismiss_always") {
    writeDevcontainerDeclined();
  }
  // Re-evaluate now that the defer barrier is gone.
  maybeAutoActivate();
}

editor.on("action_popup_result", (data) => {
  // env-manager no longer owns a trust popup; the only action popup it still
  // coordinates with is the devcontainer plugin's attach prompt.
  if (data.popup_id === DEVCONTAINER_ATTACH_POPUP_ID) {
    onDevcontainerAttachResult(data);
  }
});

// Trust is granted elsewhere — the core trust modal, the status-bar trust
// pill, or a palette command — all of which route through core's
// `set_workspace_trust_level` and fire `trust_changed`. That is our cue to
// activate a now-trusted env, so the user's single "Trust" decision both
// elevates trust and turns the env on, with no second prompt from us.
editor.on("trust_changed", (data) => {
  if (data.level === "trusted" && !editor.envActive()) {
    maybeAutoActivate();
  }
});

// === Plugin-load orchestration ===

/**
 * Decide what (if anything) to do on plugin load for the detected env.
 *
 * Routing:
 * - No env detected → nothing.
 * - Path-only (`.venv`/`venv`) → auto-activate silently if trusted. Path-only
 *   is intentionally exempt from trust prompting; the snippet is just `PATH`
 *   setup.
 * - Shell env, devcontainer present, local authority, no observed
 *   decline → defer entirely. The devcontainer attach popup goes first; we
 *   re-run after the post-attach restart inside the container, or when the
 *   user declines the devcontainer popup (see `onDevcontainerAttachResult`).
 * - Shell env, devcontainer present but user already declined the attach
 *   (this session or persistently) → fall through to the env flow on the
 *   host.
 * - Shell env, already activated → nothing (the env is live; user can reload).
 * - Shell env, trusted → silent activation (trust is the green light; honor it).
 * - Shell env, not yet trusted → nothing. The core workspace-trust modal owns
 *   the ask; when the user trusts the folder, `trust_changed` re-runs this and
 *   we activate. We never surface our own trust popup here.
 */
function maybeAutoActivate(): void {
  const det = detect();
  if (!det) return;

  if (det.kind === "path-only") {
    if (isTrusted() && !editor.envActive()) {
      applyActivation(det);
    }
    return;
  }

  // det.kind === "shell"
  if (editor.envActive()) return;
  if (devcontainerConfigPresent() && !authorityIsNonLocal()) {
    // Only defer while the user might still see the devcontainer prompt.
    // If they declined it earlier in this session, or persistently
    // declined in a previous session (so devcontainer is silently
    // skipping its popup), proceed to the env activate flow instead of
    // waiting for a popup that will never appear.
    if (devcontainerDismissedThisSession) {
      editor.debug(
        "env-manager: devcontainer dismissed this session — proceeding with env activate",
      );
    } else if (readDevcontainerDeclined()) {
      editor.debug(
        "env-manager: user previously declined devcontainer attach for this folder — proceeding with env activate",
      );
    } else {
      editor.debug(
        "env-manager: deferring to devcontainer plugin (config present, local authority)",
      );
      return;
    }
  }

  if (isTrusted()) {
    // Trust is granted (now or previously) → activate. Trust is the only gate.
    applyActivation(det);
    return;
  }
  // Not trusted yet: stay silent. The core trust modal asks; `trust_changed`
  // brings us back here once the user trusts.
}

registerHandler("env_maybe_auto_activate", maybeAutoActivate);
editor.on("plugins_loaded", "env_maybe_auto_activate");

// A session activated *after* boot — most notably one spawned through the
// Orchestrator ("New Session" / dock), which creates a window without
// re-firing `plugins_loaded` — must get the same auto-activation a direct
// `fresh <dir>` launch gets. Without this, such a session stays on the system
// toolchain even once its workspace trust is decided (issue #2355 follow-up).
// Guarded on `!envActive()` so merely switching back to an already-active
// session is a no-op and never re-applies (which would needlessly reload LSP).
registerHandler("env_auto_activate_on_session", () => {
  if (!editor.envActive()) maybeAutoActivate();
});
editor.on("active_window_changed", "env_auto_activate_on_session");

// === Status pill (opt-in to a user's status-bar layout) ===
//
// One pill: "env" — what environment is active (always relevant once
// env-manager runs). Workspace trust has its own first-class, always-visible
// core status-bar element (`{trust}`, rendered from the active session's trust
// level every frame and capitalized); env-manager no longer registers a
// competing per-buffer trust chip — that one rendered lowercase and vanished
// for sessions whose buffer hadn't been refreshed.

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
// `ready` populates the pills at boot (after workspace restore opened the
// initial buffer) so an opted-in trust chip shows immediately, not only after
// the first buffer switch / file open.
for (const event of ["ready", "buffer_activated", "after_file_open", "focus_gained"]) {
  editor.on(event, "env_refresh_status");
}

// === Clickable chip ===
//
// The `env` pill env-manager registers becomes a first-class affordance back
// to its decision. Clicking it re-runs the activate flow, re-opening the
// activate prompt if a pending decision remains — the "Status beats prompts"
// callback, where the indicator is the affordance, not just a passive label.
// (The workspace-trust `{trust}` element is core and owns its own click →
// trust prompt; env-manager no longer handles trust clicks.)
editor.on("status_bar_token_clicked", (data) => {
  if (data.plugin_name !== "env-manager") return;
  if (data.token_name === STATUS_TOKEN) {
    // Click on the env pill is an explicit "act on this env" gesture. If the
    // folder is trusted, activate (or reload). If it isn't, open the core
    // trust modal — the single trust prompt — rather than a plugin-owned
    // popup; trusting then activates via `trust_changed`. If there's no env
    // detected at all, fall back to a status message so the click isn't silent.
    const det = detect();
    if (!det) {
      editor.setStatus(editor.t("status.no_env_detected"));
      return;
    }
    if (isTrusted()) {
      applyActivation(det);
    } else {
      requestCoreTrustPrompt();
    }
  }
});

refreshStatus();
