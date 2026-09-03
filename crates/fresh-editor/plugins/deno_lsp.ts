/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Deno LSP selection
 *
 * A project whose root holds a `deno.json`/`deno.jsonc` is served by `deno
 * lsp` (#1191), but only when the Deno runtime is installed — otherwise a
 * repo that merely contains a `deno.json` loses both languages to a server
 * that cannot start (#2981).
 *
 * Two differences from the version this replaced in `configure_lsp_servers`:
 * the choice follows the active window rather than being fixed per window
 * root, and it lands when this plugin loads, so an already-spawned server
 * keeps its command until restarted (JS/TS are `auto_start: false`).
 */

const DENO_LANGUAGES = ["typescript", "javascript"] as const;

const DENO_SERVER = {
  command: "deno",
  args: ["lsp"],
  autoStart: false,
  initializationOptions: { enable: true },
  processLimits: null,
} as const;

type LspEntry = {
  command: string;
  args: string[];
  autoStart: boolean | null;
  initializationOptions: Record<string, unknown> | null;
  processLimits: null;
};

/** What each language was configured with before the first override. */
const baseline = new Map<string, LspEntry>();

let appliedRoot: string | null = null;
let appliedDeno = false;

/** Per-root decisions, so re-evaluating on every buffer switch is free. */
const decisions = new Map<string, boolean>();
let denoInstalled: boolean | null = null;

function isWindows(): boolean {
  // Truthy, not `!== null`: an unset variable comes back `undefined` here
  // and `""` elsewhere.
  if (editor.getEnv("PATHEXT")) return true;
  return /^[A-Za-z]:[\\/]/.test(editor.getCwd());
}

function denoOnPath(): boolean {
  if (denoInstalled !== null) return denoInstalled;

  const path = editor.getEnv("PATH");
  if (!path) {
    denoInstalled = false;
    return false;
  }
  const windows = isWindows();
  const entries = path.split(windows ? ";" : ":").filter((entry) => entry.length > 0);
  const names = windows ? ["deno.exe", "deno.cmd", "deno.bat"] : ["deno"];

  denoInstalled = entries.some((dir) => names.some((name) => editor.fileExists(editor.pathJoin(dir, name))));
  return denoInstalled;
}

function hasDenoConfig(root: string): boolean {
  if (!root) return false;
  return (
    editor.fileExists(editor.pathJoin(root, "deno.json")) ||
    editor.fileExists(editor.pathJoin(root, "deno.jsonc"))
  );
}

type ConfiguredEntry = {
  command?: string;
  args?: string[];
  enabled?: boolean;
  auto_start?: boolean;
  initialization_options?: Record<string, unknown> | null;
};

type LspConfig = Record<string, ConfiguredEntry | ConfiguredEntry[] | undefined>;

/** The `lsp` section of the live config, read once per pass — it is a walk over the whole config. */
function currentLsp(): LspConfig {
  const config = editor.getConfig() as { lsp?: LspConfig } | null;
  return config?.lsp ?? {};
}

function configuredEntries(lsp: LspConfig, language: string): ConfiguredEntry[] {
  const entry = lsp[language];
  if (!entry) return [];
  return Array.isArray(entry) ? entry : [entry];
}

/**
 * The server the user has switched on for `language`, or `null`.
 *
 * A language whose every entry is `enabled: false` — the "Disable" row in
 * the LSP popups writes exactly that — is one the user turned off, and
 * `registerLspServer` can only register an enabled server, so touching it
 * would turn it back on. The plugin leaves those alone in both directions.
 */
function enabledServer(lsp: LspConfig, language: string): ConfiguredEntry | null {
  return configuredEntries(lsp, language).find((entry) => entry.enabled !== false && !!entry.command) ?? null;
}

function toEntry(server: ConfiguredEntry): LspEntry {
  return {
    command: server.command ?? "",
    args: server.args ?? [],
    autoStart: server.auto_start ?? null,
    initializationOptions: server.initialization_options ?? null,
    processLimits: null,
  };
}

function installDeno(): void {
  const lsp = currentLsp();
  for (const language of DENO_LANGUAGES) {
    const server = enabledServer(lsp, language);
    if (!server) continue;
    if (!baseline.has(language)) baseline.set(language, toEntry(server));
    editor.registerLspServer(language, { ...DENO_SERVER, args: [...DENO_SERVER.args] });
  }
}

function restoreBaseline(): void {
  const lsp = currentLsp();
  for (const language of DENO_LANGUAGES) {
    const entry = baseline.get(language);
    // `undefined`: never overridden. Disabled since: the user's call.
    if (!entry || !enabledServer(lsp, language)) continue;
    editor.registerLspServer(language, { ...entry, args: [...entry.args] });
  }
}

function applyForRoot(root: string): void {
  if (appliedRoot === root) return;

  let wantsDeno = decisions.get(root);
  if (wantsDeno === undefined) {
    wantsDeno = hasDenoConfig(root) && denoOnPath();
    decisions.set(root, wantsDeno);
  }

  const changed = appliedDeno !== wantsDeno;
  appliedRoot = root;
  appliedDeno = wantsDeno;
  if (!changed) return;

  if (wantsDeno) {
    editor.debug(`deno_lsp: ${root} is a Deno project and deno is installed — using \`deno lsp\``);
    installDeno();
  } else {
    editor.debug(`deno_lsp: ${root} keeps its configured JS/TS servers`);
    restoreBaseline();
  }
}

function applyForActiveWindow(): void {
  const activeId = editor.activeWindow();
  const active = editor.listWindows().find((window) => window.id === activeId);
  if (active) applyForRoot(active.root);
}

editor.on("window_created", (data) => applyForRoot(data.root));
editor.on("active_window_changed", () => applyForActiveWindow());
editor.on("buffer_activated", () => applyForActiveWindow());

// At load rather than from `plugins_loaded`, which is not delivered in every
// embedding — and this has to have decided before a server starts.
applyForActiveWindow();
