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
 *   inside the repo. This is the dangerous case. We surface a combined
 *   "trust this folder and activate?" popup so the user makes one decision
 *   that elevates trust *and* activates the env.
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

/** Popup ids — namespaced so action_popup_result callbacks can route. */
const POPUP_ACTIVATE = "env-manager-activate";
const POPUP_TRUST_ELEVATE = "env-manager-trust-elevate";

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
   * "path-only" envs (`.venv`/`venv`) auto-activate silently.
   * "shell" envs (`.envrc`/`mise.toml`/`.tool-versions`) prompt first.
   */
  kind: "path-only" | "shell";
  /** Marker file or directory name that triggered detection (for the popup body). */
  marker: string;
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
        marker: name,
      };
    }
  }

  if (fileExists(editor.pathJoin(cwd, ".envrc"))) {
    return {
      name: "direnv",
      snippet: `eval "$(direnv export bash)"`,
      kind: "shell",
      marker: ".envrc",
    };
  }

  for (const name of ["mise.toml", ".mise.toml", ".tool-versions"]) {
    if (fileExists(editor.pathJoin(cwd, name))) {
      return {
        name: "mise",
        snippet: `eval "$(mise env -s bash)"`,
        kind: "shell",
        marker: name,
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

// === Per-cwd decision persistence ===

type EnvDecision = "activated" | "dismissed";

function envDecisionKey(): string {
  return "env-decision:" + editor.getCwd();
}

function readEnvDecision(): EnvDecision | null {
  const raw = editor.getGlobalState(envDecisionKey()) as unknown;
  if (raw === "activated" || raw === "dismissed") return raw;
  return null;
}

function writeEnvDecision(value: EnvDecision): void {
  editor.setGlobalState(envDecisionKey(), value);
}

/** Session-only "Not now" — cleared on plugin reload, so the next editor
 * restart re-asks. Separate from the persisted "Never here" decision so
 * users have a real difference between "later" and "stop asking forever". */
let envDismissedThisSession = false;

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
  writeEnvDecision("activated");
}

/** Activate (or, when already active, reload) the detected environment.
 *
 * Trust handling: if the workspace is not trusted, instead of silently
 * failing we surface a follow-up action popup ("Workspace not trusted —
 * trust and activate?") so the user can elevate trust without leaving the
 * activation flow. This replaces the previous dead-end status message.
 */
function activate(): void {
  const det = detect();
  if (!det) {
    editor.setStatus(editor.t("status.no_env_detected"));
    return;
  }
  if (!isTrusted()) {
    showTrustElevatePrompt(det);
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

// === Popups ===

/**
 * Combined trust + activate popup, surfaced on plugin load when the workspace
 * has a shell-based env (`.envrc` / `mise.toml`) and the user hasn't yet made
 * a decision. The "Trust & activate" action elevates trust *and* activates;
 * the user gets one decision for what is logically one intent.
 */
function showActivatePrompt(det: Detected): void {
  editor.showActionPopup({
    id: POPUP_ACTIVATE,
    title: editor.t("popup.activate_title"),
    message: editor.t("popup.activate_message", { name: det.name, marker: det.marker }),
    actions: [
      { id: "trust_and_activate", label: editor.t("popup.activate_action_trust") },
      { id: "dismiss_once", label: editor.t("popup.activate_action_not_now") },
      { id: "dismiss_always", label: editor.t("popup.activate_action_never") },
    ],
  });
}

/**
 * Follow-up popup shown when the user explicitly runs `Env: Activate` (or
 * clicks the locked pill) on an untrusted workspace. Same shape as the
 * combined popup but framed as an elevation request — the user already asked
 * to activate, so we just need their consent to elevate trust.
 */
function showTrustElevatePrompt(det: Detected): void {
  editor.showActionPopup({
    id: POPUP_TRUST_ELEVATE,
    title: editor.t("popup.trust_elevate_title"),
    message: editor.t("popup.trust_elevate_message", { name: det.name }),
    actions: [
      { id: "trust_and_activate", label: editor.t("popup.activate_action_trust") },
      { id: "keep_restricted", label: editor.t("popup.trust_elevate_action_keep") },
      { id: "cancel", label: editor.t("popup.trust_elevate_action_cancel") },
    ],
  });
}

/** Promote the workspace to Trusted by dispatching the existing trust action.
 * Plugins can't set trust directly through a dedicated API, but the editor
 * exposes `workspace_trust_trust` as an action and `executeActions` is the
 * generic dispatch channel. */
function elevateTrust(): void {
  editor.executeActions([{ action: "workspace_trust_trust", count: 1 }]);
}

function onActivatePopup(data: ActionPopupResultData): void {
  const det = detect();
  if (!det) return;
  if (data.action_id === "trust_and_activate") {
    elevateTrust();
    applyActivation(det);
  } else if (data.action_id === "dismiss_always") {
    writeEnvDecision("dismissed");
  } else {
    // "dismiss_once" or the generic "dismissed" id the core injects on
    // Escape — both are session-only; the next editor restart re-asks.
    envDismissedThisSession = true;
  }
}

function onTrustElevatePopup(data: ActionPopupResultData): void {
  const det = detect();
  if (!det) return;
  if (data.action_id === "trust_and_activate") {
    elevateTrust();
    applyActivation(det);
  } else if (data.action_id === "keep_restricted") {
    editor.executeActions([{ action: "workspace_trust_restrict", count: 1 }]);
    editor.setStatus(editor.t("status.kept_restricted"));
  }
  // "cancel" / "dismissed" — no-op, leaves trust as-is.
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
  if (data.popup_id === POPUP_ACTIVATE) {
    onActivatePopup(data);
  } else if (data.popup_id === POPUP_TRUST_ELEVATE) {
    onTrustElevatePopup(data);
  } else if (data.popup_id === DEVCONTAINER_ATTACH_POPUP_ID) {
    onDevcontainerAttachResult(data);
  }
});

// === Plugin-load orchestration ===

/**
 * Decide what (if anything) to do on plugin load for the detected env.
 *
 * Routing:
 * - No env detected → nothing.
 * - Path-only (`.venv`/`venv`) → auto-activate silently if trusted, regardless
 *   of any prior decision (the activation is recorded but we don't re-prompt
 *   the user about a non-prompting flow). Path-only is intentionally
 *   exempt from the trust-gating popup; the snippet is just `PATH` setup.
 * - Shell env, devcontainer present, local authority, no observed
 *   decline → defer entirely. The devcontainer attach popup goes first; we
 *   re-run after the post-attach restart inside the container, or when the
 *   user declines the devcontainer popup (see `onDevcontainerAttachResult`).
 * - Shell env, devcontainer present but user already declined the attach
 *   (this session or persistently) → fall through to the env flow on the
 *   host. Defer is only valid while the devcontainer popup might still
 *   appear; once it's clear it won't, the env popup should.
 * - Shell env, already activated → nothing (the env is live; user can reload).
 * - Shell env, prior "dismissed" decision → nothing (respect the user's "never here").
 * - Shell env, session-only dismissal → nothing this session.
 * - Shell env, undecided + trusted → silent activation (trust is the
 *   green light; honor it).
 * - Shell env, undecided + untrusted → show the combined trust+activate popup.
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

  const prior = readEnvDecision();
  if (prior === "dismissed") return;
  if (envDismissedThisSession) return;
  if (prior === "activated" && isTrusted()) {
    // User previously said yes; silently re-activate without re-prompting.
    applyActivation(det);
    return;
  }
  if (isTrusted()) {
    // Trust is already granted; just activate.
    applyActivation(det);
    return;
  }
  showActivatePrompt(det);
}

registerHandler("env_maybe_auto_activate", maybeAutoActivate);
editor.on("plugins_loaded", "env_maybe_auto_activate");

// === Status pill (opt-in to a user's status-bar layout) ===
//
// Two pills:
// - "env" — what environment is active (always relevant once env-manager runs)
// - "trust" — visible only when the workspace is *not* Trusted. This is the
//   "restricted mode is always visible" rule: silent gating without a visible
//   chip is the failure mode that gives VS Code its UX reputation. When the
//   chip is present, the user knows code execution is gated and can run
//   "Workspace Trust: Trust This Folder" (or click through the env pill
//   prompt) to elevate.

const TRUST_TOKEN = "trust";

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

  // Trust chip — show only when not Trusted. Trusted is the "everything works"
  // state and adding a chip there would just be noise.
  const level = editor.workspaceTrustLevel();
  const trustValue =
    level === "restricted"
      ? editor.t("statusbar.trust_restricted")
      : level === "blocked"
        ? editor.t("statusbar.trust_blocked")
        : "";
  editor.setStatusBarValue(bufferId, TRUST_TOKEN, trustValue);
}

editor.registerStatusBarElement(STATUS_TOKEN, editor.t("statusbar.label"));
editor.registerStatusBarElement(TRUST_TOKEN, editor.t("statusbar.trust_label"));

registerHandler("env_refresh_status", refreshStatus);
for (const event of ["buffer_activated", "after_file_open", "focus_gained"]) {
  editor.on(event, "env_refresh_status");
}

// === Clickable chips ===
//
// The status-bar pills env-manager registers (`env` and `trust`) become
// first-class affordances back to their decisions. Clicking the env pill
// re-runs `maybeAutoActivate`, which re-opens the activate prompt if a
// pending decision remains. Clicking the trust chip routes through the
// same flow but skips the silent path (the user clicked specifically
// because they want to act on trust). These are the "Status beats
// prompts" callbacks — the indicator is the affordance, not just a
// passive label.
editor.on("status_bar_token_clicked", (data) => {
  if (data.plugin_name !== "env-manager") return;
  if (data.token_name === STATUS_TOKEN) {
    // Click on the env pill is an explicit "I want to reconsider"
    // gesture, so it bypasses both the session-only "Not now" and the
    // persistent "Never here" dismissals — `maybeAutoActivate` would
    // otherwise short-circuit on the persisted state and the chip
    // would do nothing. Instead, surface the appropriate popup
    // directly based on detected env + trust state. If there's no env
    // detected at all, fall back to a status message so the click
    // isn't silent.
    const det = detect();
    if (!det) {
      editor.setStatus(editor.t("status.no_env_detected"));
      return;
    }
    envDismissedThisSession = false;
    if (isTrusted()) {
      applyActivation(det);
    } else {
      showActivatePrompt(det);
    }
  } else if (data.token_name === TRUST_TOKEN) {
    // If we have an env to activate, drive into the trust-elevation
    // popup for it; otherwise fall back to invoking the workspace
    // trust prompt directly so the user can still elevate.
    const det = detect();
    if (det && !isTrusted()) {
      showTrustElevatePrompt(det);
    } else {
      editor.executeActions([{ action: "workspace_trust_prompt", count: 1 }]);
    }
  }
});

refreshStatus();
