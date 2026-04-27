/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Dev Container Plugin
 *
 * Detects .devcontainer/devcontainer.json configurations and provides:
 * - Status bar summary of the container environment
 * - Info panel showing image, features, ports, env vars, lifecycle commands
 * - Lifecycle command runner via command palette
 * - Quick open for the devcontainer.json config file
 */

// =============================================================================
// Types
// =============================================================================

interface DevContainerConfig {
  name?: string;
  image?: string;
  build?: {
    dockerfile?: string;
    context?: string;
    args?: Record<string, string>;
    target?: string;
    cacheFrom?: string | string[];
  };
  dockerComposeFile?: string | string[];
  service?: string;
  features?: Record<string, string | boolean | Record<string, unknown>>;
  forwardPorts?: (number | string)[];
  portsAttributes?: Record<string, PortAttributes>;
  appPort?: number | string | (number | string)[];
  containerEnv?: Record<string, string>;
  remoteEnv?: Record<string, string>;
  userEnvProbe?: "none" | "loginShell" | "loginInteractiveShell" | "interactiveShell";
  containerUser?: string;
  remoteUser?: string;
  mounts?: (string | MountConfig)[];
  initializeCommand?: LifecycleCommand;
  onCreateCommand?: LifecycleCommand;
  updateContentCommand?: LifecycleCommand;
  postCreateCommand?: LifecycleCommand;
  postStartCommand?: LifecycleCommand;
  postAttachCommand?: LifecycleCommand;
  customizations?: Record<string, unknown>;
  runArgs?: string[];
  workspaceFolder?: string;
  workspaceMount?: string;
  shutdownAction?: string;
  overrideCommand?: boolean;
  init?: boolean;
  privileged?: boolean;
  capAdd?: string[];
  securityOpt?: string[];
  hostRequirements?: {
    cpus?: number;
    memory?: string;
    storage?: string;
    gpu?: boolean | string | { cores?: number; memory?: string };
  };
}

type LifecycleCommand = string | string[] | Record<string, string | string[]>;

interface PortAttributes {
  label?: string;
  protocol?: string;
  onAutoForward?: string;
  requireLocalPort?: boolean;
  elevateIfNeeded?: boolean;
}

interface MountConfig {
  type?: string;
  source?: string;
  target?: string;
}

// =============================================================================
// State
// =============================================================================

let config: DevContainerConfig | null = null;
let configPath: string | null = null;
let infoPanelBufferId: number | null = null;
let infoPanelSplitId: number | null = null;
let infoPanelOpen = false;
let cachedContent = "";

/// Single shared panel slot for every devcontainer-owned panel
/// (Show Info / Show Container Logs / Show Build Logs / Show
/// Forwarded Ports / lifecycle command output / build-log
/// streaming / failed-attach error). Without this, each `Show *`
/// invocation used to call `createVirtualBufferInSplit` directly,
/// which always creates a new horizontal split — so by the third
/// invocation the right column was several stacked panes ~5 rows
/// each, the layout became uninhabitable, and several downstream
/// L-rows in the usability bug table collapsed into "the layout
/// strategy is wrong."
///
/// Policy (matches the user's spec for fix #6 on retest):
///   - If `panelSplitId` is set AND that split still exists in
///     `editor.listSplits()`, REUSE it: focus + swap content.
///   - Otherwise, use the currently focused split — *don't*
///     spawn a new one. The first Show command therefore
///     replaces whatever is in the focused split (the user's
///     editor pane is gone if they didn't manually split first;
///     this is the explicit tradeoff the user picked over the
///     unbounded-stacking alternative).
let panelSplitId: number | null = null;
const panelBufferIds = new Set<number>();

// The in-flight `devcontainer up` handle (set before we await, cleared
// on exit). `devcontainer_cancel_attach` forwards `.kill()` to this.
// null when no attach is running.
let attachInFlight: ProcessHandle<SpawnResult> | null = null;

// Set by `devcontainer_cancel_attach` right before it kills the
// in-flight handle; read by `runDevcontainerUp` so the non-zero exit
// coming out of the kill doesn't also trigger a FailedAttach — the
// cancel already set the indicator back to Local.
let attachCancelled = false;

// Focus state for info panel buttons (Tab navigation like pkg.ts)
type InfoFocusTarget = { type: "button"; index: number };

interface InfoButton {
  id: string;
  label: string;
  command: string;
}

const infoButtons: InfoButton[] = [
  { id: "run", label: "Run Lifecycle", command: "devcontainer_run_lifecycle" },
  { id: "open", label: "Open Config", command: "devcontainer_open_config" },
  { id: "rebuild", label: "Rebuild", command: "devcontainer_rebuild" },
  { id: "close", label: "Close", command: "devcontainer_close_info" },
];

let infoFocus: InfoFocusTarget = { type: "button", index: 0 };

// =============================================================================
// Colors
// =============================================================================

const colors = {
  heading: [255, 200, 100] as [number, number, number],
  key: [100, 200, 255] as [number, number, number],
  value: [200, 200, 200] as [number, number, number],
  feature: [150, 255, 150] as [number, number, number],
  port: [255, 180, 100] as [number, number, number],
  footer: [120, 120, 120] as [number, number, number],
  button: [180, 180, 190] as [number, number, number],
  buttonFocused: [255, 255, 255] as [number, number, number],
  buttonFocusedBg: [60, 110, 180] as [number, number, number],
};

// =============================================================================
// Config Discovery
// =============================================================================

/// Last parse failure observed by `findConfig` — surfaced via
/// `setStatus` and an action popup at init / on file save so the
/// user notices broken JSON instead of silently losing every
/// `Dev Container:` command.
let lastParseError: { path: string; message: string } | null = null;

function tryParse(path: string, content: string): boolean {
  try {
    config = editor.parseJsonc(content) as DevContainerConfig;
    configPath = path;
    lastParseError = null;
    return true;
  } catch (e) {
    const message = e instanceof Error ? e.message : String(e);
    lastParseError = { path, message };
    // Set `configPath` to the broken file so the recovery
    // command set's `Open Config` can route the user there.
    // `config` stays null so callers that depend on parsed
    // fields (rebuild, attach) don't crash.
    configPath = path;
    editor.debug(`devcontainer: failed to parse ${path}: ${message}`);
    return false;
  }
}

function findConfig(): boolean {
  const cwd = editor.getCwd();
  lastParseError = null;

  // Priority 1: .devcontainer/devcontainer.json
  const primary = editor.pathJoin(cwd, ".devcontainer", "devcontainer.json");
  const primaryContent = editor.readFile(primary);
  if (primaryContent !== null) {
    if (tryParse(primary, primaryContent)) return true;
  }

  // Priority 2: .devcontainer.json
  const secondary = editor.pathJoin(cwd, ".devcontainer.json");
  const secondaryContent = editor.readFile(secondary);
  if (secondaryContent !== null) {
    if (tryParse(secondary, secondaryContent)) return true;
  }

  // Priority 3: .devcontainer/<subfolder>/devcontainer.json
  const dcDir = editor.pathJoin(cwd, ".devcontainer");
  if (editor.fileExists(dcDir)) {
    const entries = editor.readDir(dcDir);
    for (const entry of entries) {
      if (entry.is_dir) {
        const subConfig = editor.pathJoin(dcDir, entry.name, "devcontainer.json");
        const subContent = editor.readFile(subConfig);
        if (subContent !== null) {
          if (tryParse(subConfig, subContent)) return true;
        }
      }
    }
  }

  return false;
}

/// Surface the last parse error (if any) to the user via the status
/// bar. Idempotent — safe to call repeatedly. Bug #2 (silent JSON
/// syntax errors): without this, a broken `devcontainer.json`
/// causes `findConfig` to return false, no commands register,
/// and the user has no clue why the feature stopped working.
function showParseErrorIfAny(): void {
  if (!lastParseError) return;
  editor.setStatus(
    editor.t("status.parse_failed", {
      path: lastParseError.path,
      message: lastParseError.message,
    }),
  );
}

// =============================================================================
// Formatting Helpers
// =============================================================================

function formatLifecycleCommand(cmd: LifecycleCommand): string {
  if (typeof cmd === "string") return cmd;
  if (Array.isArray(cmd)) return cmd.join(" ");
  return Object.entries(cmd)
    .map(([k, v]) => `${k}: ${Array.isArray(v) ? v.join(" ") : v}`)
    .join("; ");
}

function formatMount(mount: string | MountConfig): string {
  if (typeof mount === "string") return mount;
  const parts: string[] = [];
  if (mount.source) parts.push(mount.source);
  parts.push("->");
  if (mount.target) parts.push(mount.target);
  if (mount.type) parts.push(`(${mount.type})`);
  return parts.join(" ");
}

function getImageSummary(): string {
  if (!config) return "unknown";
  if (config.image) return config.image;
  if (config.build?.dockerfile) return "Dockerfile: " + config.build.dockerfile;
  if (config.dockerComposeFile) return "Compose";
  return "unknown";
}

// =============================================================================
// Info Panel
// =============================================================================

function buildInfoEntries(): TextPropertyEntry[] {
  if (!config) return [];

  const entries: TextPropertyEntry[] = [];

  // Header
  const name = config.name ?? "unnamed";
  entries.push({
    text: editor.t("panel.header", { name }) + "\n",
    properties: { type: "heading" },
  });
  entries.push({ text: "\n", properties: { type: "blank" } });

  // Image / Build / Compose
  if (config.image) {
    entries.push({ text: editor.t("panel.section_image") + "\n", properties: { type: "heading" } });
    entries.push({ text: "  " + config.image + "\n", properties: { type: "value" } });
    entries.push({ text: "\n", properties: { type: "blank" } });
  } else if (config.build?.dockerfile) {
    entries.push({ text: editor.t("panel.section_build") + "\n", properties: { type: "heading" } });
    entries.push({ text: "  dockerfile: " + config.build.dockerfile + "\n", properties: { type: "value" } });
    if (config.build.context) {
      entries.push({ text: "  context: " + config.build.context + "\n", properties: { type: "value" } });
    }
    if (config.build.target) {
      entries.push({ text: "  target: " + config.build.target + "\n", properties: { type: "value" } });
    }
    entries.push({ text: "\n", properties: { type: "blank" } });
  } else if (config.dockerComposeFile) {
    entries.push({ text: editor.t("panel.section_compose") + "\n", properties: { type: "heading" } });
    const files = Array.isArray(config.dockerComposeFile)
      ? config.dockerComposeFile.join(", ")
      : config.dockerComposeFile;
    entries.push({ text: "  files: " + files + "\n", properties: { type: "value" } });
    if (config.service) {
      entries.push({ text: "  service: " + config.service + "\n", properties: { type: "value" } });
    }
    entries.push({ text: "\n", properties: { type: "blank" } });
  }

  // Features
  if (config.features && Object.keys(config.features).length > 0) {
    entries.push({ text: editor.t("panel.section_features") + "\n", properties: { type: "heading" } });
    for (const [id, opts] of Object.entries(config.features)) {
      entries.push({ text: "  + " + id + "\n", properties: { type: "feature", id } });
      if (typeof opts === "object" && opts !== null) {
        const optStr = Object.entries(opts as Record<string, unknown>)
          .map(([k, v]) => `${k} = ${JSON.stringify(v)}`)
          .join(", ");
        if (optStr) {
          entries.push({ text: "      " + optStr + "\n", properties: { type: "feature-opts" } });
        }
      }
    }
    entries.push({ text: "\n", properties: { type: "blank" } });
  }

  // Ports
  if (config.forwardPorts && config.forwardPorts.length > 0) {
    entries.push({ text: editor.t("panel.section_ports") + "\n", properties: { type: "heading" } });
    for (const port of config.forwardPorts) {
      const attrs = config.portsAttributes?.[String(port)];
      const proto = attrs?.protocol ?? "tcp";
      let detail = `  ${port} -> ${proto}`;
      if (attrs?.label) detail += ` (${attrs.label})`;
      if (attrs?.onAutoForward) detail += ` [${attrs.onAutoForward}]`;
      entries.push({ text: detail + "\n", properties: { type: "port", port: String(port) } });
    }
    entries.push({ text: "\n", properties: { type: "blank" } });
  }

  // Environment
  const allEnv: Record<string, string> = {};
  if (config.containerEnv) Object.assign(allEnv, config.containerEnv);
  if (config.remoteEnv) Object.assign(allEnv, config.remoteEnv);
  const envKeys = Object.keys(allEnv);
  if (envKeys.length > 0) {
    entries.push({ text: editor.t("panel.section_env") + "\n", properties: { type: "heading" } });
    for (const k of envKeys) {
      entries.push({ text: `  ${k} = ${allEnv[k]}\n`, properties: { type: "env" } });
    }
    entries.push({ text: "\n", properties: { type: "blank" } });
  }

  // Mounts
  if (config.mounts && config.mounts.length > 0) {
    entries.push({ text: editor.t("panel.section_mounts") + "\n", properties: { type: "heading" } });
    for (const mount of config.mounts) {
      entries.push({ text: "  " + formatMount(mount) + "\n", properties: { type: "mount" } });
    }
    entries.push({ text: "\n", properties: { type: "blank" } });
  }

  // Users
  if (config.containerUser || config.remoteUser) {
    entries.push({ text: editor.t("panel.section_users") + "\n", properties: { type: "heading" } });
    if (config.containerUser) {
      entries.push({ text: "  containerUser: " + config.containerUser + "\n", properties: { type: "value" } });
    }
    if (config.remoteUser) {
      entries.push({ text: "  remoteUser: " + config.remoteUser + "\n", properties: { type: "value" } });
    }
    entries.push({ text: "\n", properties: { type: "blank" } });
  }

  // Lifecycle Commands
  const lifecycle: [string, LifecycleCommand | undefined][] = [
    ["initializeCommand", config.initializeCommand],
    ["onCreateCommand", config.onCreateCommand],
    ["updateContentCommand", config.updateContentCommand],
    ["postCreateCommand", config.postCreateCommand],
    ["postStartCommand", config.postStartCommand],
    ["postAttachCommand", config.postAttachCommand],
  ];
  const defined = lifecycle.filter(([, v]) => v !== undefined);
  if (defined.length > 0) {
    entries.push({ text: editor.t("panel.section_lifecycle") + "\n", properties: { type: "heading" } });
    for (const [cmdName, cmd] of defined) {
      entries.push({
        text: `  ${cmdName}: ${formatLifecycleCommand(cmd!)}\n`,
        properties: { type: "lifecycle", command: cmdName },
      });
    }
    entries.push({ text: "\n", properties: { type: "blank" } });
  }

  // Host Requirements
  if (config.hostRequirements) {
    const hr = config.hostRequirements;
    entries.push({ text: editor.t("panel.section_host_req") + "\n", properties: { type: "heading" } });
    if (hr.cpus) entries.push({ text: `  cpus: ${hr.cpus}\n`, properties: { type: "value" } });
    if (hr.memory) entries.push({ text: `  memory: ${hr.memory}\n`, properties: { type: "value" } });
    if (hr.storage) entries.push({ text: `  storage: ${hr.storage}\n`, properties: { type: "value" } });
    if (hr.gpu) entries.push({ text: `  gpu: ${JSON.stringify(hr.gpu)}\n`, properties: { type: "value" } });
    entries.push({ text: "\n", properties: { type: "blank" } });
  }

  // Separator before buttons
  entries.push({
    text: "─".repeat(40) + "\n",
    properties: { type: "separator" },
  });

  // Action buttons row (Tab-navigable, like pkg.ts)
  entries.push({ text: " ", properties: { type: "spacer" } });
  for (let i = 0; i < infoButtons.length; i++) {
    const btn = infoButtons[i];
    const focused = infoFocus.index === i;
    const leftBracket = focused ? "[" : " ";
    const rightBracket = focused ? "]" : " ";
    entries.push({
      text: `${leftBracket} ${btn.label} ${rightBracket}`,
      properties: { type: "button", focused, btnIndex: i },
    });
    if (i < infoButtons.length - 1) {
      entries.push({ text: " ", properties: { type: "spacer" } });
    }
  }
  entries.push({ text: "\n", properties: { type: "newline" } });

  // Help line
  entries.push({
    text: editor.t("panel.footer") + "\n",
    properties: { type: "footer" },
  });

  return entries;
}

function entriesToContent(entries: TextPropertyEntry[]): string {
  return entries.map((e) => e.text).join("");
}

function applyInfoHighlighting(): void {
  if (infoPanelBufferId === null) return;
  const bufferId = infoPanelBufferId;

  editor.clearNamespace(bufferId, "devcontainer");

  const content = cachedContent;
  if (!content) return;

  const lines = content.split("\n");
  let byteOffset = 0;

  for (let lineIdx = 0; lineIdx < lines.length; lineIdx++) {
    const line = lines[lineIdx];
    const lineStart = byteOffset;
    const lineByteLen = editor.utf8ByteLength(line);
    const lineEnd = lineStart + lineByteLen;

    // Heading lines (sections)
    if (
      line.startsWith("Dev Container:") ||
      line === editor.t("panel.section_image") ||
      line === editor.t("panel.section_build") ||
      line === editor.t("panel.section_compose") ||
      line === editor.t("panel.section_features") ||
      line === editor.t("panel.section_ports") ||
      line === editor.t("panel.section_env") ||
      line === editor.t("panel.section_mounts") ||
      line === editor.t("panel.section_users") ||
      line === editor.t("panel.section_lifecycle") ||
      line === editor.t("panel.section_host_req")
    ) {
      editor.addOverlay(bufferId, "devcontainer", lineStart, lineEnd, {
        fg: colors.heading,
        bold: true,
      });
    }
    // Feature lines
    else if (line.startsWith("  + ")) {
      editor.addOverlay(bufferId, "devcontainer", lineStart, lineEnd, {
        fg: colors.feature,
      });
    }
    // Port lines
    else if (line.match(/^\s+\d+\s*->/)) {
      editor.addOverlay(bufferId, "devcontainer", lineStart, lineEnd, {
        fg: colors.port,
      });
    }
    // Key = value lines (env vars)
    else if (line.match(/^\s+\w+\s*=/)) {
      const eqIdx = line.indexOf("=");
      if (eqIdx > 0) {
        const keyEnd = lineStart + editor.utf8ByteLength(line.substring(0, eqIdx));
        editor.addOverlay(bufferId, "devcontainer", lineStart, keyEnd, {
          fg: colors.key,
        });
      }
    }
    // Separator
    else if (line.match(/^─+$/)) {
      editor.addOverlay(bufferId, "devcontainer", lineStart, lineEnd, {
        fg: colors.footer,
      });
    }
    // Footer help line
    else if (line === editor.t("panel.footer")) {
      editor.addOverlay(bufferId, "devcontainer", lineStart, lineEnd, {
        fg: colors.footer,
        italic: true,
      });
    }

    byteOffset += lineByteLen + 1; // +1 for newline
  }

  // Apply button highlighting using entry-based scanning
  // We need to walk entries to find button text positions in the content
  applyButtonHighlighting();
}

function applyButtonHighlighting(): void {
  if (infoPanelBufferId === null) return;
  const bufferId = infoPanelBufferId;

  // Re-scan entries to find button positions
  const entries = buildInfoEntries();
  let byteOffset = 0;

  for (const entry of entries) {
    const props = entry.properties as Record<string, unknown>;
    const len = editor.utf8ByteLength(entry.text);

    if (props.type === "button") {
      const focused = props.focused as boolean;
      if (focused) {
        editor.addOverlay(bufferId, "devcontainer", byteOffset, byteOffset + len, {
          fg: colors.buttonFocused,
          bg: colors.buttonFocusedBg,
          bold: true,
        });
      } else {
        editor.addOverlay(bufferId, "devcontainer", byteOffset, byteOffset + len, {
          fg: colors.button,
        });
      }
    }

    byteOffset += len;
  }
}

function updateInfoPanel(): void {
  if (infoPanelBufferId === null) return;
  const entries = buildInfoEntries();
  cachedContent = entriesToContent(entries);
  editor.setVirtualBufferContent(infoPanelBufferId, entries);
  applyInfoHighlighting();
}

// =============================================================================
// Mode Definition
// =============================================================================

editor.defineMode(
  "devcontainer-info",
  [
    ["Tab", "devcontainer_next_button"],
    ["S-Tab", "devcontainer_prev_button"],
    ["Return", "devcontainer_activate_button"],
    ["M-r", "devcontainer_run_lifecycle"],
    ["M-o", "devcontainer_open_config"],
    ["M-b", "devcontainer_rebuild"],
    ["q", "devcontainer_close_info"],
    ["Escape", "devcontainer_close_info"],
  ],
  true, // read-only
  false, // allow_text_input
  true, // inherit Normal-context bindings so arrow keys / page nav still work
);

// =============================================================================
// Info Panel Button Navigation
// =============================================================================

// Plugin code runs inside an IIFE, so `function foo() {}` declarations don't
// land on globalThis on their own. Register each handler explicitly so it can
// be referenced by string name from defineMode bindings, registerCommand, and
// event handlers (see also pkg.ts).

function devcontainer_next_button(): void {
  if (!infoPanelOpen) return;
  infoFocus = { type: "button", index: (infoFocus.index + 1) % infoButtons.length };
  updateInfoPanel();
}
registerHandler("devcontainer_next_button", devcontainer_next_button);

function devcontainer_prev_button(): void {
  if (!infoPanelOpen) return;
  infoFocus = { type: "button", index: (infoFocus.index - 1 + infoButtons.length) % infoButtons.length };
  updateInfoPanel();
}
registerHandler("devcontainer_prev_button", devcontainer_prev_button);

function devcontainer_activate_button(): void {
  if (!infoPanelOpen) return;
  const btn = infoButtons[infoFocus.index];
  if (!btn) return;
  const handler = (globalThis as Record<string, unknown>)[btn.command];
  if (typeof handler === "function") {
    (handler as () => void)();
  }
}
registerHandler("devcontainer_activate_button", devcontainer_activate_button);

// =============================================================================
// Commands
// =============================================================================

async function devcontainer_show_info(): Promise<void> {
  if (!config) {
    editor.setStatus(editor.t("status.no_config"));
    return;
  }

  // Re-routed through the shared panel slot (Bug #6 retest):
  // dropping the previous flag-based dedupe because it kept the
  // info-panel buffer "open" in module state even when the user
  // had already closed it with `q`, leaving the next invocation
  // either refreshing a dead buffer or stacking a new split.
  // The slot helper handles existence-checking against the live
  // split list each call.
  infoFocus = { type: "button", index: 0 };
  const entries = buildInfoEntries();
  cachedContent = entriesToContent(entries);

  const result = await openVirtualInPanelSlot({
    name: "*Dev Container*",
    mode: "devcontainer-info",
    entries,
  });

  if (result !== null) {
    infoPanelOpen = true;
    infoPanelBufferId = result.bufferId;
    infoPanelSplitId = result.splitId;
    applyInfoHighlighting();
    editor.setStatus(editor.t("status.panel_opened"));
  }
}
registerHandler("devcontainer_show_info", devcontainer_show_info);

function devcontainer_close_info(): void {
  if (!infoPanelOpen) return;

  if (infoPanelSplitId !== null) {
    editor.closeSplit(infoPanelSplitId);
  }
  if (infoPanelBufferId !== null) {
    editor.closeBuffer(infoPanelBufferId);
  }

  infoPanelOpen = false;
  infoPanelBufferId = null;
  infoPanelSplitId = null;
  editor.setStatus(editor.t("status.panel_closed"));
}
registerHandler("devcontainer_close_info", devcontainer_close_info);

function devcontainer_open_config(): void {
  if (configPath) {
    editor.openFile(configPath, null, null);
  } else {
    editor.setStatus(editor.t("status.no_config"));
  }
}
registerHandler("devcontainer_open_config", devcontainer_open_config);

function devcontainer_run_lifecycle(): void {
  if (!config) {
    editor.setStatus(editor.t("status.no_config"));
    return;
  }

  // `initializeCommand` is the host-side prologue per the dev-container
  // spec — surface it in the picker so users can re-run it on demand.
  // The automatic attach flow runs it separately (see runDevcontainerUp)
  // before `devcontainer up`, so the CLI-driven hooks that follow don't
  // re-run it.
  const lifecycle: [string, LifecycleCommand | undefined][] = [
    ["initializeCommand", config.initializeCommand],
    ["onCreateCommand", config.onCreateCommand],
    ["updateContentCommand", config.updateContentCommand],
    ["postCreateCommand", config.postCreateCommand],
    ["postStartCommand", config.postStartCommand],
    ["postAttachCommand", config.postAttachCommand],
  ];

  const defined = lifecycle.filter(([, v]) => v !== undefined);
  if (defined.length === 0) {
    editor.setStatus(editor.t("status.no_lifecycle"));
    return;
  }

  const suggestions: PromptSuggestion[] = defined.map(([name, cmd]) => ({
    text: name,
    description: formatLifecycleCommand(cmd!),
    value: name,
  }));

  editor.startPrompt(editor.t("prompt.run_lifecycle"), "devcontainer-lifecycle");
  editor.setPromptSuggestions(suggestions);
}
registerHandler("devcontainer_run_lifecycle", devcontainer_run_lifecycle);

async function devcontainer_on_lifecycle_confirmed(data: {
  prompt_type: string;
  input: string;
}): Promise<void> {
  if (data.prompt_type !== "devcontainer-lifecycle") return;

  const cmdName = data.input;
  if (!config || !cmdName) return;

  const cmd = (config as Record<string, unknown>)[cmdName] as LifecycleCommand | undefined;
  if (!cmd) return;

  // cwd: when attached to a Container, pass the in-container
  // `remoteWorkspaceFolder` so `docker exec -w` lands inside
  // the container. When local, pass "" — the runtime treats
  // empty-string cwd the same as omitted (both fall back to
  // working_dir). Avoids passing literal `undefined` through
  // the QuickJS bridge, which the marshaller rejects with
  // "Error converting from js 'undefined' into type 'string'".
  const cwd = lifecycleCwd() ?? "";
  const env = await effectiveLifecycleEnv();
  if (typeof cmd === "string") {
    editor.setStatus(editor.t("status.running", { name: cmdName }));
    const [bin, args] = wrapWithEnv(env, "sh", ["-c", cmd]);
    const result = await editor.spawnProcess(bin, args, cwd);
    await surfaceLifecycleResult(cmdName, null, cmd, result);
  } else if (Array.isArray(cmd)) {
    const [origBin, ...origArgs] = cmd;
    const [bin, args] = wrapWithEnv(env, origBin, origArgs);
    editor.setStatus(editor.t("status.running", { name: cmdName }));
    const result = await editor.spawnProcess(bin, args, cwd);
    await surfaceLifecycleResult(cmdName, null, [origBin, ...origArgs].join(" "), result);
  } else {
    // Object form: see the rewritten parallel branch in
    // `runLifecycleObjectForm`.
    await runLifecycleObjectForm(cmdName, cmd);
  }
}

/// Critical bug from interactive walkthrough: lifecycle command
/// stdout/stderr were captured in `result.stdout` / `result.stderr`
/// and then discarded — the user only saw a status line like
/// `postCreateCommand failed (exit 1)` with zero way to see the
/// real error. Now we surface the full output via the shared
/// panel slot.
///
/// Always render (even on success) so users can see what the
/// command actually did. Status line keeps the at-a-glance
/// summary; the panel carries the detail.
async function surfaceLifecycleResult(
  cmdName: string,
  label: string | null,
  cmdline: string,
  result: { stdout: string; stderr: string; exit_code: number },
): Promise<void> {
  // Status line: the at-a-glance signal. Detail lands in the
  // panel slot below.
  if (result.exit_code === 0) {
    editor.setStatus(editor.t("status.completed", { name: cmdName }));
  } else if (label !== null) {
    editor.setStatus(
      editor.t("status.failed_sub", {
        name: cmdName,
        label,
        code: String(result.exit_code),
      }),
    );
  } else {
    editor.setStatus(
      editor.t("status.failed", {
        name: cmdName,
        code: String(result.exit_code),
      }),
    );
  }

  const headerLine = label !== null
    ? `--- ${cmdName} (${label}) — exit ${result.exit_code} ---\n`
    : `--- ${cmdName} — exit ${result.exit_code} ---\n`;
  const cmdLineText = `$ ${cmdline}\n`;
  const stdoutBlock = result.stdout.length > 0 ? result.stdout : "";
  const stderrBlock = result.stderr.length > 0
    ? (result.stdout.length > 0 ? "\n--- stderr ---\n" : "") + result.stderr
    : "";
  const body = stdoutBlock + stderrBlock;
  const text = headerLine + cmdLineText
    + (body.length > 0 ? body : "(no output)\n");

  await openVirtualInPanelSlot({
    name: "*Dev Container Lifecycle*",
    mode: "devcontainer-info",
    entries: [{ text, properties: { type: "log" } }],
  });
}

/// Per-workspace storage for `remoteWorkspaceFolder` captured at
/// attach time. The plugin module re-loads after `setAuthority`'s
/// restart, losing in-memory state, so we persist via plugin
/// global state. Read back via `lifecycleCwd()` when running
/// lifecycle commands.
function remoteWorkspaceKey(): string {
  return "remote-workspace:" + editor.getCwd();
}

function writeRemoteWorkspace(value: string | null): void {
  editor.setGlobalState(remoteWorkspaceKey(), value);
}

function readRemoteWorkspace(): string | null {
  const raw = editor.getGlobalState(remoteWorkspaceKey()) as unknown;
  return typeof raw === "string" && raw.length > 0 ? raw : null;
}

/// Pick the cwd to pass to lifecycle-command `spawnProcess` calls.
/// When attached to a Container authority, returns the recorded
/// `remoteWorkspaceFolder` so `docker exec -w` lands inside the
/// container. Otherwise returns undefined so the runtime fills
/// in the editor's host working_dir (the local-authority path).
function lifecycleCwd(): string | undefined {
  if (editor.getAuthorityLabel().startsWith("Container:")) {
    return readRemoteWorkspace() ?? undefined;
  }
  return undefined;
}

/// Per-workspace cache of the `userEnvProbe` result. Spec says
/// the tool runs the probe shell once at attach and applies the
/// captured env to every subsequent remote process. We persist
/// across the post-attach restart via plugin global state so the
/// reloaded plugin instance reuses the same snapshot.
function userEnvProbeKey(): string {
  return "user-env-probe:" + editor.getCwd();
}

function readCachedProbedEnv(): Record<string, string> | null {
  const raw = editor.getGlobalState(userEnvProbeKey()) as unknown;
  if (raw && typeof raw === "object" && !Array.isArray(raw)) {
    const out: Record<string, string> = {};
    for (const [k, v] of Object.entries(raw as Record<string, unknown>)) {
      if (typeof v === "string") out[k] = v;
    }
    return out;
  }
  return null;
}

function writeCachedProbedEnv(env: Record<string, string>): void {
  editor.setGlobalState(userEnvProbeKey(), env as unknown);
}

/// Run the `userEnvProbe` shell (per spec) and capture its env.
/// Caches the result so subsequent calls are free. Returns `{}`
/// when probe is unset / "none" / failed.
async function getOrComputeProbedEnv(): Promise<Record<string, string>> {
  const cached = readCachedProbedEnv();
  if (cached !== null) return cached;

  const probe = config?.userEnvProbe;
  if (!probe || probe === "none") {
    writeCachedProbedEnv({});
    return {};
  }

  // Map enum → bash flags. `loginShell` = `bash -lc`,
  // `interactiveShell` = `bash -ic`, etc. The probe runs `env`
  // and we parse stdout into KEY=VALUE pairs.
  const flagMap: Record<string, string[]> = {
    loginShell: ["-l"],
    loginInteractiveShell: ["-l", "-i"],
    interactiveShell: ["-i"],
  };
  const flags = flagMap[probe] ?? [];
  const cwd = lifecycleCwd() ?? "";
  // The probe shell needs `remoteEnv` applied too so users can put
  // BASH_ENV / ENV / NODE_OPTIONS / etc. there and have the probe
  // pick them up. Without this, bash's non-interactive-login
  // semantics (`BASH_ENV` sourcing) wouldn't see the user's
  // configured rc file.
  const baseEnv: Record<string, string> = config?.remoteEnv ?? {};
  const [bin, probeArgs] = wrapWithEnv(baseEnv, "bash", [...flags, "-c", "env"]);
  const result = await editor.spawnProcess(bin, probeArgs, cwd);
  if (result.exit_code !== 0) {
    editor.debug(
      `devcontainer: userEnvProbe (${probe}) failed: ${result.stderr.trim()}`,
    );
    writeCachedProbedEnv({});
    return {};
  }
  const env: Record<string, string> = {};
  for (const line of result.stdout.split("\n")) {
    const eq = line.indexOf("=");
    if (eq > 0) {
      env[line.slice(0, eq)] = line.slice(eq + 1);
    }
  }
  writeCachedProbedEnv(env);
  return env;
}

/// Build the merged env passed to lifecycle commands per spec:
///   userEnvProbe-captured ∪ remoteEnv (remoteEnv overrides probe).
/// Skipped when not attached to a Container — remoteEnv is a
/// container-side concept, the local case relies on whatever env
/// the editor itself has.
async function effectiveLifecycleEnv(): Promise<Record<string, string>> {
  if (!editor.getAuthorityLabel().startsWith("Container:")) return {};
  const probed = await getOrComputeProbedEnv();
  const out: Record<string, string> = { ...probed };
  if (config?.remoteEnv) {
    for (const [k, v] of Object.entries(config.remoteEnv)) {
      out[k] = v;
    }
  }
  return out;
}

/// Probe the just-launched container's user-shell env so the docker
/// authority's spawner can apply it to every `docker exec` (including
/// the LSP `command_exists` probe and the LSP server spawn itself).
///
/// Why pre-restart, not post-restart: `setAuthority` rebuilds the
/// editor in place; the next plugin instance can't influence the
/// already-installed authority's spawner without a second restart.
/// We have one shot, here, while we still hold the host-side spawner
/// and know which container we're talking to.
///
/// Why bash login-interactive: per the dev-container spec, the
/// default `userEnvProbe` is `loginInteractiveShell`. `bash -lic env`
/// matches what an attached terminal would see — including PATH
/// additions from `~/.profile`, `~/.bashrc`, and friends. Custom
/// `userEnvProbe` settings (`loginShell`, `interactiveShell`, `none`)
/// are honoured.
///
/// Failures (no bash, probe times out, exit non-zero) degrade
/// gracefully: we return `[]` and the user gets the bare
/// container-default PATH back — same behaviour as before this fix,
/// no regression.
async function captureContainerLoginEnv(
  result: DevcontainerUpResult,
): Promise<Array<[string, string]>> {
  if (!result.containerId) return [];
  const probe = config?.userEnvProbe ?? "loginInteractiveShell";
  if (probe === "none") return [];

  const flagMap: Record<string, string[]> = {
    loginShell: ["-l"],
    loginInteractiveShell: ["-l", "-i"],
    interactiveShell: ["-i"],
  };
  const flags = flagMap[probe];
  if (!flags) return [];

  // Compose `docker exec -i [-u USER] [-w WORKSPACE] <id> bash ...`
  // by hand: at this point the authority hasn't been installed yet,
  // so `editor.spawnProcess` would route to the host. Use
  // `spawnHostProcess` and address the container directly via the
  // host docker CLI.
  const dockerArgs: string[] = ["exec", "-i"];
  if (result.remoteUser) {
    dockerArgs.push("-u", result.remoteUser);
  }
  if (result.remoteWorkspaceFolder) {
    dockerArgs.push("-w", result.remoteWorkspaceFolder);
  }
  dockerArgs.push(result.containerId, "bash", ...flags, "-c", "env");

  const probeResult = await editor.spawnHostProcess("docker", dockerArgs);
  if (probeResult.exit_code !== 0) {
    editor.debug(
      `devcontainer: container userEnvProbe (${probe}) failed exit=${probeResult.exit_code}: ${probeResult.stderr.trim()}`,
    );
    return [];
  }

  // Filter to a small allowlist of high-value entries. Forwarding the
  // entire `env` dump risks shadowing useful container defaults
  // (`HOSTNAME`, `_`, …) and balloons the `docker exec` arg list.
  // `PATH` is the one that drives the LSP fix; the others are common
  // setup the user's shell exports and a non-shell exec wouldn't.
  const wanted = new Set(["PATH", "HOME", "LANG", "LC_ALL", "SHELL", "USER", "LOGNAME"]);
  const out: Array<[string, string]> = [];
  for (const line of probeResult.stdout.split("\n")) {
    const eq = line.indexOf("=");
    if (eq <= 0) continue;
    const key = line.slice(0, eq);
    if (!wanted.has(key)) continue;
    out.push([key, line.slice(eq + 1)]);
  }
  return out;
}

/// Wrap `[bin, args]` with an `env K1=V1 K2=V2 bin args...`
/// invocation when `env` is non-empty. Returns the original pair
/// when env is empty (no wrapper needed).
///
/// Note: GNU `env` doesn't recognize `--` as an options
/// terminator (it dies with `env: '--': No such file or directory`).
/// `env` parses K=V pairs greedily until it hits a non-K=V word,
/// which it treats as the command. As long as `bin` doesn't
/// contain `=`, this is unambiguous.
function wrapWithEnv(
  env: Record<string, string>,
  bin: string,
  args: string[],
): [string, string[]] {
  const keys = Object.keys(env);
  if (keys.length === 0) return [bin, args];
  const envArgs = keys.map((k) => `${k}=${env[k]}`);
  return ["env", [...envArgs, bin, ...args]];
}

/// Spec: object-form lifecycle commands run their entries in
/// parallel; the stage waits for all to complete; the stage
/// succeeds iff every entry exited 0. Implementation:
/// `Promise.all` over an array of per-entry promises, each
/// reporting its exit code. We aggregate failures into a single
/// status message at the end.
async function runLifecycleObjectForm(
  cmdName: string,
  cmd: Record<string, string | string[]>,
): Promise<void> {
  const entries = Object.entries(cmd);
  if (entries.length === 0) {
    editor.setStatus(editor.t("status.completed", { name: cmdName }));
    return;
  }
  editor.setStatus(editor.t("status.running", { name: cmdName }));

  const cwd = lifecycleCwd() ?? "";
  const env = await effectiveLifecycleEnv();
  const results = await Promise.all(
    entries.map(async ([label, subcmd]) => {
      let origBin: string;
      let origArgs: string[];
      let cmdline: string;
      if (Array.isArray(subcmd)) {
        [origBin, ...origArgs] = subcmd;
        cmdline = [origBin, ...origArgs].join(" ");
      } else {
        origBin = "sh";
        origArgs = ["-c", subcmd as string];
        cmdline = subcmd as string;
      }
      const [bin, args] = wrapWithEnv(env, origBin, origArgs);
      const r = await editor.spawnProcess(bin, args, cwd);
      return { label, cmdline, result: r };
    }),
  );

  // Render every entry's output into the panel slot in one
  // batched message — N separate calls would flicker the panel
  // across N intermediate states. Failed entries first so the
  // user sees the failures even if stdout is enormous.
  const sorted = [...results].sort(
    (a, b) => Number(a.result.exit_code === 0) - Number(b.result.exit_code === 0),
  );
  const sections = sorted.map(({ label, cmdline, result: r }) => {
    const header = `--- ${cmdName} (${label}) — exit ${r.exit_code} ---\n`;
    const cmdLine = `$ ${cmdline}\n`;
    const stdoutBlock = r.stdout.length > 0 ? r.stdout : "";
    const stderrBlock = r.stderr.length > 0
      ? (r.stdout.length > 0 ? "\n--- stderr ---\n" : "") + r.stderr
      : "";
    const body = stdoutBlock + stderrBlock;
    return header + cmdLine + (body.length > 0 ? body : "(no output)\n");
  });
  await openVirtualInPanelSlot({
    name: "*Dev Container Lifecycle*",
    mode: "devcontainer-info",
    entries: [{ text: sections.join("\n"), properties: { type: "log" } }],
  });

  const failed = results.filter((r) => r.result.exit_code !== 0);
  if (failed.length === 0) {
    editor.setStatus(editor.t("status.completed", { name: cmdName }));
    return;
  }
  // Surface the first failure in the status message — same key
  // the old sequential path used so existing translations keep
  // working.
  const first = failed[0];
  editor.setStatus(
    editor.t("status.failed_sub", {
      name: cmdName,
      label: first.label,
      code: String(first.result.exit_code),
    }),
  );
}
registerHandler("devcontainer_on_lifecycle_confirmed", devcontainer_on_lifecycle_confirmed);

function devcontainer_show_features(): void {
  if (!config || !config.features || Object.keys(config.features).length === 0) {
    editor.setStatus(editor.t("status.no_features"));
    return;
  }

  const suggestions: PromptSuggestion[] = Object.entries(config.features).map(([id, opts]) => {
    let desc = "";
    if (typeof opts === "object" && opts !== null) {
      desc = Object.entries(opts as Record<string, unknown>)
        .map(([k, v]) => `${k}=${JSON.stringify(v)}`)
        .join(", ");
    } else if (typeof opts === "string") {
      desc = opts;
    }
    return { text: id, description: desc || "(default options)" };
  });

  editor.startPrompt(editor.t("prompt.features"), "devcontainer-features");
  editor.setPromptSuggestions(suggestions);
}
registerHandler("devcontainer_show_features", devcontainer_show_features);

/// Parse `docker port <id>` output into a map from
/// "<container-port>/<proto>" to "<host>:<host-port>".
///
/// Each output line looks like `8080/tcp -> 0.0.0.0:49153`. Malformed
/// lines are skipped — we prefer a partial merge over bailing on
/// unknown formats from future Docker versions.
function parseDockerPortOutput(stdout: string): Record<string, string> {
  const map: Record<string, string> = {};
  for (const rawLine of stdout.split("\n")) {
    const line = rawLine.trim();
    if (!line) continue;
    const arrow = line.indexOf(" -> ");
    if (arrow < 0) continue;
    const left = line.slice(0, arrow).trim();
    const right = line.slice(arrow + 4).trim();
    if (left && right) map[left] = right;
  }
  return map;
}

async function devcontainer_show_ports(): Promise<void> {
  if (!config || !config.forwardPorts || config.forwardPorts.length === 0) {
    editor.setStatus(editor.t("status.no_ports"));
    return;
  }

  // When attached to a container, merge runtime bindings from
  // `docker port <id>` into the prompt descriptions so the user sees
  // which configured ports actually reached the host. Off-container
  // the runtime side is unavailable; fall back to config-only.
  let runtime: Record<string, string> = {};
  const authorityLabel = editor.getAuthorityLabel();
  const prefix = "Container:";
  if (authorityLabel.startsWith(prefix)) {
    const containerId = authorityLabel.slice(prefix.length);
    if (containerId.length > 0) {
      const which = await editor.spawnHostProcess("which", ["docker"]);
      if (which.exit_code === 0) {
        const res = await editor.spawnHostProcess(
          "docker",
          ["port", containerId],
          editor.getCwd(),
        );
        if (res.exit_code === 0) {
          runtime = parseDockerPortOutput(res.stdout);
        }
      }
    }
  }

  const suggestions: PromptSuggestion[] = config.forwardPorts.map((port) => {
    const attrs = config!.portsAttributes?.[String(port)];
    const proto = attrs?.protocol ?? "tcp";
    let desc = proto;
    if (attrs?.label) desc += ` · ${attrs.label}`;
    if (attrs?.onAutoForward) desc += ` (${attrs.onAutoForward})`;
    // Runtime bindings are keyed by "<port>/<protocol>" — Docker
    // emits `tcp` / `udp` lowercased. Match protocol defensively.
    const key = `${port}/${proto.toLowerCase()}`;
    const binding = runtime[key];
    if (binding) {
      desc += ` → ${binding}`;
    }
    return { text: String(port), description: desc };
  });

  // Surface runtime-only ports (exposed by the container but not
  // listed in forwardPorts) so users see the full picture.
  for (const [key, binding] of Object.entries(runtime)) {
    const slash = key.indexOf("/");
    const portStr = slash >= 0 ? key.slice(0, slash) : key;
    const portNum = Number(portStr);
    const alreadyListed =
      config.forwardPorts.some((p) => String(p) === portStr) ||
      (!Number.isNaN(portNum) && config.forwardPorts.some((p) => p === portNum));
    if (alreadyListed) continue;
    suggestions.push({
      text: portStr,
      description: `${key} · runtime only → ${binding}`,
    });
  }

  editor.startPrompt(editor.t("prompt.ports"), "devcontainer-ports");
  editor.setPromptSuggestions(suggestions);
}
registerHandler("devcontainer_show_ports", devcontainer_show_ports);

// =============================================================================
// Forwarded Ports Panel (spec §7)
// =============================================================================
//
// Phase A's `devcontainer_show_ports` is a prompt-picker: quick
// lookups for "did this port actually bind?" E-3 extends that with a
// standalone panel so users can see configured + runtime-bound ports
// at a glance rather than scrolling a picker.
//
// Data sources (identical to the picker):
//   - `config.forwardPorts` — declared port forwards
//   - `config.portsAttributes` — optional label / protocol / policy
//   - `docker port <id>` — runtime host binding per (port, proto)
//
// Layout: four columns — Configured | Protocol | Label | Runtime binding —
// followed by any runtime-only ports (container exposed but not in
// `forwardPorts`). Refresh key `r` re-runs `docker port` and rebuilds
// the buffer. Close via `q` / Escape.

let portsPanelBufferId: number | null = null;
let portsPanelSplitId: number | null = null;
let portsPanelOpen = false;

type PortRow = {
  port: string;
  protocol: string;
  label: string;
  binding: string;
  source: "configured" | "runtime";
};

async function gatherForwardedPortRows(): Promise<PortRow[]> {
  let runtime: Record<string, string> = {};
  const authorityLabel = editor.getAuthorityLabel();
  const prefix = "Container:";
  if (authorityLabel.startsWith(prefix)) {
    const containerId = authorityLabel.slice(prefix.length);
    if (containerId.length > 0) {
      const which = await editor.spawnHostProcess("which", ["docker"]);
      if (which.exit_code === 0) {
        const res = await editor.spawnHostProcess(
          "docker",
          ["port", containerId],
          editor.getCwd(),
        );
        if (res.exit_code === 0) {
          runtime = parseDockerPortOutput(res.stdout);
        }
      }
    }
  }

  const rows: PortRow[] = [];
  const configured = config?.forwardPorts ?? [];
  for (const port of configured) {
    const attrs = config?.portsAttributes?.[String(port)];
    const protocol = attrs?.protocol ?? "tcp";
    const key = `${port}/${protocol.toLowerCase()}`;
    const binding = runtime[key] ?? "";
    const labelParts: string[] = [];
    if (attrs?.label) labelParts.push(attrs.label);
    if (attrs?.onAutoForward) labelParts.push(`(${attrs.onAutoForward})`);
    rows.push({
      port: String(port),
      protocol,
      label: labelParts.join(" "),
      binding,
      source: "configured",
    });
  }

  // Runtime-only ports: the container exposed them but they aren't in
  // `forwardPorts`. Worth surfacing so users see the full picture.
  for (const [key, binding] of Object.entries(runtime)) {
    const slash = key.indexOf("/");
    const portStr = slash >= 0 ? key.slice(0, slash) : key;
    const proto = slash >= 0 ? key.slice(slash + 1) : "tcp";
    const portNum = Number(portStr);
    const alreadyListed =
      configured.some((p) => String(p) === portStr) ||
      (!Number.isNaN(portNum) && configured.some((p) => p === portNum));
    if (alreadyListed) continue;
    rows.push({
      port: portStr,
      protocol: proto,
      label: "",
      binding,
      source: "runtime",
    });
  }
  return rows;
}

function buildPortsPanelEntries(rows: PortRow[]): TextPropertyEntry[] {
  const entries: TextPropertyEntry[] = [];

  entries.push({
    text: editor.t("ports_panel.header") + "\n",
    properties: { type: "heading" },
  });
  entries.push({ text: "\n", properties: { type: "blank" } });

  if (rows.length === 0) {
    entries.push({
      text: "  " + editor.t("ports_panel.no_ports") + "\n",
      properties: { type: "value" },
    });
    entries.push({ text: "\n", properties: { type: "blank" } });
  } else {
    // Column widths — pick the larger of the header width or the
    // longest value so the header stays aligned even when all rows
    // are shorter than the label.
    const headers = {
      port: editor.t("ports_panel.col_configured"),
      protocol: editor.t("ports_panel.col_protocol"),
      label: editor.t("ports_panel.col_label"),
      binding: editor.t("ports_panel.col_binding"),
    };
    const width = (label: string, values: string[]): number =>
      Math.max(label.length, ...values.map((v) => v.length));
    const portW = width(
      headers.port,
      rows.map((r) => r.port),
    );
    const protoW = width(
      headers.protocol,
      rows.map((r) => r.protocol),
    );
    const labelW = width(
      headers.label,
      rows.map((r) => r.label),
    );
    const bindingW = width(
      headers.binding,
      rows.map((r) => r.binding),
    );
    const pad = (s: string, n: number): string =>
      s + " ".repeat(Math.max(0, n - s.length));

    const headerLine =
      "  " +
      pad(headers.port, portW) +
      "  " +
      pad(headers.protocol, protoW) +
      "  " +
      pad(headers.label, labelW) +
      "  " +
      pad(headers.binding, bindingW);
    entries.push({
      text: headerLine + "\n",
      properties: { type: "heading" },
    });
    const rule =
      "  " +
      "─".repeat(portW) +
      "  " +
      "─".repeat(protoW) +
      "  " +
      "─".repeat(labelW) +
      "  " +
      "─".repeat(bindingW);
    entries.push({
      text: rule + "\n",
      properties: { type: "separator" },
    });

    for (const row of rows) {
      const rendered =
        "  " +
        pad(row.port, portW) +
        "  " +
        pad(row.protocol, protoW) +
        "  " +
        pad(row.label, labelW) +
        "  " +
        pad(row.binding || "—", bindingW);
      entries.push({
        text: rendered + "\n",
        properties: { type: "port-row", source: row.source },
      });
    }
    entries.push({ text: "\n", properties: { type: "blank" } });
  }

  entries.push({
    text: editor.t("ports_panel.footer") + "\n",
    properties: { type: "footer" },
  });

  return entries;
}

async function renderPortsPanel(): Promise<void> {
  if (portsPanelBufferId === null) return;
  const rows = await gatherForwardedPortRows();
  const entries = buildPortsPanelEntries(rows);
  editor.setVirtualBufferContent(portsPanelBufferId, entries);
}

async function devcontainer_show_forwarded_ports_panel(): Promise<void> {
  if (!config) {
    editor.setStatus(editor.t("status.no_config"));
    return;
  }

  // Bug #6 retest: route through the shared panel slot rather
  // than `createVirtualBufferInSplit` (which always splits) and
  // drop the flag-based dedupe (which left state stale when the
  // user closed the panel manually with `q`).
  const rows = await gatherForwardedPortRows();
  const entries = buildPortsPanelEntries(rows);
  const result = await openVirtualInPanelSlot({
    name: "*Dev Container Ports*",
    mode: "devcontainer-ports",
    entries,
  });
  if (result !== null) {
    portsPanelOpen = true;
    portsPanelBufferId = result.bufferId;
    portsPanelSplitId = result.splitId;
    editor.setStatus(editor.t("status.ports_panel_opened"));
  }
}
registerHandler(
  "devcontainer_show_forwarded_ports_panel",
  devcontainer_show_forwarded_ports_panel,
);

async function devcontainer_refresh_ports_panel(): Promise<void> {
  if (!portsPanelOpen) return;
  await renderPortsPanel();
  editor.setStatus(editor.t("status.ports_panel_refreshed"));
}
registerHandler(
  "devcontainer_refresh_ports_panel",
  devcontainer_refresh_ports_panel,
);

function devcontainer_close_ports_panel(): void {
  if (!portsPanelOpen) return;
  if (portsPanelSplitId !== null) {
    editor.closeSplit(portsPanelSplitId);
  }
  if (portsPanelBufferId !== null) {
    editor.closeBuffer(portsPanelBufferId);
  }
  portsPanelOpen = false;
  portsPanelBufferId = null;
  portsPanelSplitId = null;
}
registerHandler(
  "devcontainer_close_ports_panel",
  devcontainer_close_ports_panel,
);

editor.defineMode(
  "devcontainer-ports",
  [
    ["r", "devcontainer_refresh_ports_panel"],
    ["q", "devcontainer_close_ports_panel"],
    ["Escape", "devcontainer_close_ports_panel"],
  ],
  true, // read-only
  false, // allow_text_input
  true, // inherit Normal-context bindings so arrow keys / page nav still work
);

const INSTALL_COMMAND = "npm i -g @devcontainers/cli";

interface ActionPopupResultData {
  popup_id: string;
  action_id: string;
}

function showCliNotFoundPopup(): void {
  editor.showActionPopup({
    id: "devcontainer-cli-help",
    title: editor.t("popup.cli_title"),
    message: editor.t("popup.cli_message"),
    actions: [
      { id: "copy_install", label: "Copy: " + INSTALL_COMMAND },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
}

function devcontainer_on_action_result(data: ActionPopupResultData): void {
  if (data.popup_id === "devcontainer-cli-help") {
    switch (data.action_id) {
      case "copy_install":
        editor.setClipboard(INSTALL_COMMAND);
        editor.setStatus(editor.t("status.copied_install", { cmd: INSTALL_COMMAND }));
        break;
      case "dismiss":
      case "dismissed":
        break;
    }
    return;
  }
  if (data.popup_id === "devcontainer-attach") {
    devcontainer_on_attach_popup(data);
    return;
  }
  if (data.popup_id === "devcontainer-failed-attach") {
    devcontainer_on_failed_attach_popup(data);
  }
}
registerHandler("devcontainer_on_action_result", devcontainer_on_action_result);

/// Surface a proactive action popup after a failed attach so users
/// don't have to notice the Remote Indicator's red state on their own.
/// Spec §8 calls for "Retry" / "Reopen Locally" on build failure; we
/// also offer "Show Build Logs" (the file is still on disk — see
/// `prepareBuildLogFile`) and a "Dismiss" escape so the user can come
/// back later via the Remote Indicator menu without the popup blocking.
///
/// All four actions map to existing handlers:
///   - Retry → `devcontainer_retry_attach`
///   - Show Build Logs → `devcontainer_show_build_logs`
///   - Reopen Locally → `clearRemoteIndicatorState` (no authority was
///     installed, so nothing to detach; just drop the red override).
///   - Dismiss → no-op; FailedAttach indicator stays so the user can
///     revisit the choice from the Remote Indicator popup.
function showFailedAttachPopup(errText: string): void {
  editor.showActionPopup({
    id: "devcontainer-failed-attach",
    title: editor.t("popup.failed_attach_title"),
    message: editor.t("popup.failed_attach_message", { error: errText }),
    actions: [
      { id: "retry", label: editor.t("popup.failed_attach_action_retry") },
      {
        id: "show_build_logs",
        label: editor.t("popup.failed_attach_action_show_logs"),
      },
      {
        id: "reopen_local",
        label: editor.t("popup.failed_attach_action_reopen_local"),
      },
      { id: "dismiss", label: editor.t("popup.failed_attach_action_dismiss") },
    ],
  });
}

function devcontainer_on_failed_attach_popup(data: ActionPopupResultData): void {
  if (data.popup_id !== "devcontainer-failed-attach") return;
  switch (data.action_id) {
    case "retry":
      void devcontainer_retry_attach();
      break;
    case "show_build_logs":
      void devcontainer_show_build_logs();
      break;
    case "reopen_local":
      // No authority was installed — failed attach never got that far —
      // so there is nothing to detach. Just drop the FailedAttach
      // override so the indicator returns to Local.
      editor.clearRemoteIndicatorState();
      break;
    case "dismiss":
    case "dismissed":
      // Leave the FailedAttach indicator visible so the user can revisit
      // via the Remote Indicator popup later.
      break;
  }
}
registerHandler(
  "devcontainer_on_failed_attach_popup",
  devcontainer_on_failed_attach_popup,
);

/// Convenience wrapper: flip the indicator to FailedAttach, set the
/// rebuild-failed status message, and surface the proactive action
/// popup in one call. Every branch in `runDevcontainerUp` that reaches
/// the failure state routes through here so the popup surfaces
/// consistently regardless of which step failed.
function enterFailedAttach(errText: string): void {
  editor.setStatus(editor.t("status.rebuild_failed", { error: errText }));
  editor.setRemoteIndicatorState({
    kind: "failed_attach",
    error: errText,
  });
  showFailedAttachPopup(errText);
}

// =============================================================================
// Authority lifecycle
// =============================================================================
//
// "Attach" = run `devcontainer up` on the host and install a container
// authority via editor.setAuthority({...}). The authority transition
// restarts the editor so every cached filesystem handle / LSP / PTY
// gets recreated against the new backend. We use spawnHostProcess for
// the CLI call so that a plugin triggering rebuild from inside an
// already-attached session still runs on the host, not inside the
// container that is about to be destroyed.

interface DevcontainerUpResult {
  outcome?: string;
  containerId?: string;
  remoteUser?: string;
  remoteWorkspaceFolder?: string;
}

function parseDevcontainerUpOutput(stdout: string): DevcontainerUpResult | null {
  const lines = stdout.split("\n");
  for (let i = lines.length - 1; i >= 0; i--) {
    const line = lines[i].trim();
    if (!line.startsWith("{")) continue;
    try {
      return JSON.parse(line) as DevcontainerUpResult;
    } catch {
      continue;
    }
  }
  return null;
}

function buildContainerAuthorityPayload(
  result: DevcontainerUpResult,
  baseEnv: Array<[string, string]>,
): AuthorityPayload | null {
  if (!result.containerId) return null;
  const user = result.remoteUser ?? null;
  const workspace = result.remoteWorkspaceFolder ?? null;

  const args: string[] = ["exec", "-it"];
  if (user) {
    args.push("-u", user);
  }
  if (workspace) {
    args.push("-w", workspace);
  }
  args.push(result.containerId, "bash", "-l");

  const shortId = result.containerId.slice(0, 12);

  return {
    filesystem: { kind: "local" },
    spawner: {
      kind: "docker-exec",
      container_id: result.containerId,
      user,
      workspace,
      env: baseEnv,
    },
    terminal_wrapper: {
      kind: "explicit",
      command: "docker",
      args,
      manages_cwd: true,
    },
    display_label: "Container:" + shortId,
  };
}

/// Run `initializeCommand` on the host before container lifecycle
/// hooks. Per the dev-container spec this is the "host-side
/// prologue" — it runs before `devcontainer up` and has no
/// container to be in. The `devcontainer` CLI does not invoke it
/// automatically; Fresh is the layer that has to.
///
/// Returns `true` on success or when no initializeCommand is defined;
/// `false` and sets a user-visible failure status when the command
/// exits non-zero, so callers can short-circuit the attach.
async function runInitializeCommand(): Promise<boolean> {
  const cmd = config?.initializeCommand;
  if (!cmd) {
    return true;
  }

  editor.setStatus(editor.t("status.running", { name: "initializeCommand" }));
  const cwd = editor.getCwd();

  async function runOne(bin: string, args: string[]): Promise<number> {
    const res = await editor.spawnHostProcess(bin, args, cwd);
    return res.exit_code;
  }

  let exitCode: number;
  if (typeof cmd === "string") {
    exitCode = await runOne("sh", ["-c", cmd]);
  } else if (Array.isArray(cmd)) {
    const [bin, ...rest] = cmd;
    exitCode = await runOne(bin, rest);
  } else {
    // Object form: run each named subcommand sequentially, bail on
    // first failure. Matches the semantics of the per-hook runner
    // in devcontainer_on_lifecycle_confirmed below.
    exitCode = 0;
    for (const [label, subcmd] of Object.entries(cmd)) {
      let bin: string;
      let args: string[];
      if (Array.isArray(subcmd)) {
        [bin, ...args] = subcmd;
      } else {
        bin = "sh";
        args = ["-c", subcmd as string];
      }
      editor.setStatus(
        editor.t("status.running_sub", { name: "initializeCommand", label }),
      );
      const res = await runOne(bin, args);
      if (res !== 0) {
        exitCode = res;
        editor.setStatus(
          editor.t("status.failed_sub", {
            name: "initializeCommand",
            label,
            code: String(res),
          }),
        );
        return false;
      }
    }
  }

  if (exitCode !== 0) {
    editor.setStatus(
      editor.t("status.failed", {
        name: "initializeCommand",
        code: String(exitCode),
      }),
    );
    return false;
  }
  return true;
}

async function runDevcontainerUp(extraArgs: string[]): Promise<void> {
  const cwd = editor.getCwd();
  const which = await editor.spawnHostProcess("which", ["devcontainer"]);
  if (which.exit_code !== 0) {
    showCliNotFoundPopup();
    return;
  }

  // The Remote Indicator goes into "Connecting · <phase>" for the
  // duration of the attach so users see progress; cleared (or
  // replaced with FailedAttach) by the explicit transitions below.
  editor.setRemoteIndicatorState({
    kind: "connecting",
    label: editor.t("indicator.phase_initialize"),
  });

  // initializeCommand runs on the host BEFORE `devcontainer up`, per
  // spec. Bail the attach if it fails; the user shouldn't get an
  // attached container after their host-side prologue errored.
  if (!(await runInitializeCommand())) {
    enterFailedAttach(editor.t("indicator.error_initialize"));
    return;
  }

  editor.setRemoteIndicatorState({
    kind: "connecting",
    label: editor.t("indicator.phase_build"),
  });
  editor.setStatus(editor.t("status.rebuilding"));

  // Redirect `devcontainer up`'s stderr into a workspace-scoped log
  // file; let stdout flow back through the existing pipe so we parse
  // the success JSON from `result.stdout` as before. This mirrors
  // the CLI's stream contract: stdout = machine-readable result;
  // stderr = human-readable progress / errors. The log file holds
  // exactly the "progress/errors" half.
  //
  // Rationale for the file:
  //   - "Show Build Logs" is just `openFile(path)` — no new API.
  //   - Fresh's auto-revert (2s poll) streams lines into the buffer
  //     as they arrive; user sees live progress without special
  //     plumbing.
  //   - Path is under the workspace, so bind-mount coincidence keeps
  //     it reachable post-attach (container auth sees the same file).
  //   - `.fresh-cache/.gitignore = *` self-ignores the cache dir
  //     without forcing users to touch their own `.gitignore`.
  const logPath = await prepareBuildLogFile(cwd);
  if (!logPath) {
    enterFailedAttach(editor.t("status.build_log_prepare_failed"));
    return;
  }
  rememberLastBuildLogPath(logPath);
  // Drop any session-restored build logs from previous runs before
  // opening the fresh one. Without this, `Show Build Logs` after a
  // cold restart would race the freshly-minted timestamp file against
  // a stale one in another split, with no visual cue which is which.
  closeStaleBuildLogBuffers(cwd);
  // Open the log in a split below so the user sees lines stream in
  // (auto-revert polls every 2s) without losing the buffer they were
  // editing. `split_horizontal` duplicates the current buffer into a
  // new split and focuses it; openFile then swaps the new split's
  // buffer for the log. Non-fatal if either step fails — the build
  // continues either way.
  openBuildLogInSplit(logPath);

  // `sh -c 'exec devcontainer "$@" 2> "$LOG"' sh <log> <args...>` —
  // positional-arg form so the log path and cwd never get
  // string-interpolated into the script body. $1 is the log path;
  // `shift` drops it; `$@` is the devcontainer invocation.
  const shellScript = 'LOG="$1"; shift; exec devcontainer "$@" 2> "$LOG"';
  const args = [
    "-c",
    shellScript,
    "sh",
    logPath,
    "up",
    "--workspace-folder",
    cwd,
    ...extraArgs,
  ];
  const handle = editor.spawnHostProcess("sh", args);
  attachInFlight = handle;
  attachCancelled = false;
  let result: SpawnResult;
  try {
    result = await handle;
  } finally {
    attachInFlight = null;
  }

  // Cancel path: `devcontainer_cancel_attach` set `attachCancelled`
  // and flipped the indicator back to Local already. The non-zero
  // exit coming out of `Child::start_kill()` is not an error.
  if (attachCancelled) {
    attachCancelled = false;
    return;
  }

  if (result.exit_code !== 0) {
    // On failure the log file holds the stderr trace — surface its
    // last non-empty line as a human-readable status blurb. This
    // is purely cosmetic; exit_code drove the branch.
    const logText = editor.readFile(logPath) ?? "";
    const errText = extractLastNonEmptyLine(logText)
      ?? `exit ${result.exit_code}`;
    enterFailedAttach(errText);
    return;
  }

  const parsed = parseDevcontainerUpOutput(result.stdout);
  if (!parsed || parsed.outcome !== "success" || !parsed.containerId) {
    enterFailedAttach(editor.t("status.rebuild_parse_failed"));
    return;
  }

  // Capture the in-container `userEnvProbe` env BEFORE we hand the
  // payload to setAuthority. setAuthority restarts the editor; once
  // the new editor's spawner is wired with the captured PATH, LSP
  // `command_exists` and `spawn_stdio` see the same binaries the
  // user's interactive shell sees (e.g. `pylsp` installed by a
  // `postCreateCommand` into `~/.local/bin`). Per spec, when
  // `userEnvProbe` is unset the default is `loginInteractiveShell`.
  const baseEnv = await captureContainerLoginEnv(parsed);

  const payload = buildContainerAuthorityPayload(parsed, baseEnv);
  if (!payload) {
    enterFailedAttach(editor.t("status.rebuild_missing_container_id"));
    return;
  }

  // setAuthority fires the restart flow in core. The status message
  // we set here won't survive the restart; the plugin will re-init
  // with the new authority active and print status.detected again.
  //
  // Write the attempt breadcrumb immediately before so the post-
  // restart plugin instance can detect "attach was in flight" and
  // decide between success (container authority live) and silent
  // failure (no authority landed — surfaces as FailedAttach).
  writeAttachAttempt();
  // Persist `remoteWorkspaceFolder` so the post-restart plugin
  // instance can pass it as the cwd to lifecycle commands. The
  // runtime's `spawnProcess` auto-fills working_dir (host path)
  // when cwd is omitted — that breaks `docker exec -w` for
  // configs whose `workspaceFolder` differs from the host
  // workspace path. See `lifecycleCwd()`.
  writeRemoteWorkspace(parsed.remoteWorkspaceFolder ?? null);
  editor.setAuthority(payload);
}

// Lay out `.fresh-cache/devcontainer-logs/<timestamp>.log` under the
// workspace. Returns the log path on success, null on failure
// (mkdir denied, etc.). The directory carries its own
// `.gitignore = *` so the cache never leaks into a commit without
// the user touching their top-level `.gitignore`.
async function prepareBuildLogFile(cwd: string): Promise<string | null> {
  const cacheDir = `${cwd}/.fresh-cache`;
  const logDir = `${cacheDir}/devcontainer-logs`;
  const mkRes = await editor.spawnHostProcess("mkdir", ["-p", logDir]);
  if (mkRes.exit_code !== 0) {
    editor.debug(
      `devcontainer: mkdir -p ${logDir} failed: ${mkRes.stderr.trim()}`,
    );
    return null;
  }
  const cacheIgnore = `${cacheDir}/.gitignore`;
  if (editor.readFile(cacheIgnore) === null) {
    // writeFile failure is non-fatal — worst case the user sees
    // `.fresh-cache/` in `git status` once.
    editor.writeFile(cacheIgnore, "*\n");
  }
  // `toISOString()` → "2026-04-21T12:34:56.789Z"; strip the ms+Z
  // and swap separators that are awkward in filenames on some
  // platforms.
  const ts = new Date()
    .toISOString()
    .replace(/\.\d+Z$/, "")
    .replace(/:/g, "-")
    .replace("T", "_");
  return `${logDir}/build-${ts}.log`;
}

function lastBuildLogKey(): string {
  return "last-build-log:" + editor.getCwd();
}

/// Open the build log file in a horizontal split below the current
/// pane, leaving whatever the user was editing in the top split. Used
/// both during the live build (so users see progress without losing
/// their working buffer) and from `devcontainer_show_build_logs` so
/// the post-attach access path doesn't replace the user's file
/// either.
///
/// Dedupe uses `BufferInfo.splits` from `listBuffers()` — if the log
/// is already visible in some split, focus that split. Otherwise
/// split + openFile. Reading the current snapshot each call (rather
/// than tracking split ids in module state) means the dedupe
/// survives the post-attach editor restart: after setAuthority
/// rebuilds the editor and workspace restore brings the log buffer
/// back, the first `Show Build Logs` finds the restored split and
/// focuses it instead of stacking a new one on top.
/// Resolve the split id to drop a panel into. If we have a
/// previously-claimed panel split that's still alive, reuse it.
/// Otherwise grab the currently focused split — never spawn a
/// new one. Returns the chosen split id.
function resolvePanelSplit(): number {
  if (panelSplitId !== null) {
    const stillAlive = editor.listSplits().some((s) => s.id === panelSplitId);
    if (stillAlive) return panelSplitId;
    panelSplitId = null;
  }
  const active = editor.getActiveSplitId();
  panelSplitId = active;
  return active;
}

/// Per-name registry of panel buffers. Keyed by the
/// `*Dev Container*` / `*Dev Container Logs*` etc. names so
/// re-running a Show command refreshes its own buffer in place
/// without destroying the other Show commands' buffers. Lives
/// in module state — resets on plugin reload, which is fine
/// since buffer ids change across editor restarts anyway and
/// the next Show * invocation will (re)create as needed.
const namedPanelBuffers = new Map<string, number>();

/// Drop a virtual buffer into the shared panel slot.
///
/// Per-name dedupe: re-running the same Show command refreshes
/// the existing same-name buffer in place (`setVirtualBufferContent`)
/// rather than creating a new one. Different Show commands keep
/// their own buffers — `Show Container Logs` no longer wipes out
/// `Show Container Info`'s buffer, so the user can switch back to
/// it via the tab bar / buffer list.
///
/// Returns the new buffer id, or null if the runtime call failed.
async function openVirtualInPanelSlot(opts: {
  name: string;
  mode: string;
  entries: TextPropertyEntry[];
  readOnly?: boolean;
  showLineNumbers?: boolean;
  showCursors?: boolean;
  editingDisabled?: boolean;
  lineWrap?: boolean;
}): Promise<{ bufferId: number; splitId: number } | null> {
  const splitId = resolvePanelSplit();
  const liveBuffers = editor.listBuffers();

  // If we already have a same-name buffer (and it's still alive),
  // refresh its contents and surface it in the panel split rather
  // than minting a duplicate. This is the per-command-buffer fix:
  // running `Show Container Logs` twice updates one buffer instead
  // of stacking two; running it then `Show Container Info` keeps
  // both alive.
  const existingId = namedPanelBuffers.get(opts.name);
  if (existingId !== undefined) {
    const existing = liveBuffers.find((b) => b.id === existingId);
    if (existing) {
      editor.setVirtualBufferContent(
        existing.id,
        opts.entries as unknown as Record<string, unknown>[],
      );
      editor.focusSplit(splitId);
      editor.showBuffer(existing.id);
      panelBufferIds.add(existing.id);
      return { bufferId: existing.id, splitId };
    }
    namedPanelBuffers.delete(opts.name);
  }

  const result = await editor.createVirtualBufferInExistingSplit({
    name: opts.name,
    splitId,
    mode: opts.mode,
    readOnly: opts.readOnly ?? true,
    showLineNumbers: opts.showLineNumbers ?? false,
    showCursors: opts.showCursors ?? true,
    editingDisabled: opts.editingDisabled ?? true,
    lineWrap: opts.lineWrap ?? false,
    entries: opts.entries,
  });
  if (result === null) return null;
  panelBufferIds.add(result.bufferId);
  namedPanelBuffers.set(opts.name, result.bufferId);
  return { bufferId: result.bufferId, splitId };
}

/// File-backed equivalent for build-log files: focuses the
/// panel-slot split and opens the file there. `openFile` is
/// idempotent by path, so re-running `Show Build Logs` for the
/// same file just brings the existing buffer to the front; we
/// don't need our own dedupe layer.
///
/// We also turn line-wrap off — build logs are wide structured
/// output (docker buildx, lifecycle stdout) that's much more
/// readable scrolled horizontally than soft-wrapped at column 80.
function openFileInPanelSlot(path: string): void {
  const splitId = resolvePanelSplit();
  editor.focusSplit(splitId);
  editor.openFile(path, null, null);
  // After the file is open, find its buffer id and disable
  // line wrap. The new-buffer case sets this on first open;
  // subsequent invocations are no-ops.
  const opened = editor.listBuffers().find((b) => b.path === path);
  if (opened) {
    editor.setLineWrap(opened.id, null, false);
    panelBufferIds.add(opened.id);
  }
}

function openBuildLogInSplit(path: string): void {
  openFileInPanelSlot(path);
}

/// Close every open build-log buffer for this workspace before the new
/// attach mints its own log. Without this, a session-restored buffer
/// (whose contents are stale from the previous run) sits next to the
/// fresh streaming log and the user has to guess which one is live.
///
/// Pure heuristic: any buffer whose path lives under
/// `<cwd>/.fresh-cache/devcontainer-logs/` is a build log. The
/// directory is plugin-owned (see `prepareBuildLogFile`), so the
/// false-positive surface is empty unless a user puts arbitrary files
/// there themselves — at which point closing them on attach is also
/// the right call.
function closeStaleBuildLogBuffers(cwd: string): void {
  const prefix = editor.pathJoin(cwd, ".fresh-cache", "devcontainer-logs");
  const buffers = editor.listBuffers();
  for (const b of buffers) {
    if (b.path && b.path.startsWith(prefix)) {
      editor.closeBuffer(b.id);
    }
  }
}

function rememberLastBuildLogPath(path: string): void {
  editor.setGlobalState(lastBuildLogKey(), path);
}

function readLastBuildLogPath(): string | null {
  const raw = editor.getGlobalState(lastBuildLogKey()) as unknown;
  return typeof raw === "string" && raw.length > 0 ? raw : null;
}

function extractLastNonEmptyLine(text: string): string | null {
  const lines = text.split("\n");
  for (let i = lines.length - 1; i >= 0; i--) {
    const t = lines[i].trim();
    if (t.length > 0) return t;
  }
  return null;
}

async function devcontainer_attach(): Promise<void> {
  if (!config) {
    editor.setStatus(editor.t("status.no_config"));
    return;
  }
  await runDevcontainerUp([]);
}
registerHandler("devcontainer_attach", devcontainer_attach);

async function devcontainer_rebuild(): Promise<void> {
  if (!config) {
    editor.setStatus(editor.t("status.no_config"));
    return;
  }
  await runDevcontainerUp(["--remove-existing-container"]);
}
registerHandler("devcontainer_rebuild", devcontainer_rebuild);

/// Retry a previously-failed attach. Thin wrapper around
/// `devcontainer_attach` — exists so the Remote Indicator popup's
/// FailedAttach branch can dispatch something named `retry_attach`
/// without hard-coding an implementation detail. Also the natural
/// single call site if we ever want to add backoff or attempt
/// counting.
async function devcontainer_retry_attach(): Promise<void> {
  // Drop the stale FailedAttach state before the new attempt so
  // the popup shows the freshly-entered Connecting state
  // immediately; setRemoteIndicatorState inside runDevcontainerUp
  // will override again.
  editor.clearRemoteIndicatorState();
  await devcontainer_attach();
}
registerHandler("devcontainer_retry_attach", devcontainer_retry_attach);

async function devcontainer_detach(): Promise<void> {
  // Honor `shutdownAction` per spec: default for image/Dockerfile
  // is `stopContainer`. Stop the container BEFORE clearing
  // authority — clearing the authority drops our spawner, so we'd
  // lose the easy way to issue `docker stop` against the right
  // daemon. Use `spawnHostProcess` because the container is about
  // to disappear; routing through the soon-to-be-cleared container
  // authority makes no sense.
  await stopContainerIfShutdownActionRequires();
  editor.clearAuthority();
}
registerHandler("devcontainer_detach", devcontainer_detach);

/// If `shutdownAction` says to stop the container (default for
/// image/Dockerfile), spawn `docker stop <id>` on the host.
/// No-op for `none` / `stopCompose` (compose has its own
/// teardown the plugin doesn't drive). Failures are logged but
/// don't block the detach itself — the user's intent is to stop
/// using the container, and forcing them to keep it because
/// `docker stop` errored would be worse than leaving an orphan.
async function stopContainerIfShutdownActionRequires(): Promise<void> {
  const action = config?.shutdownAction ?? "stopContainer";
  if (action !== "stopContainer") return;

  const label = editor.getAuthorityLabel();
  const prefix = "Container:";
  if (!label.startsWith(prefix)) return;
  const containerId = label.slice(prefix.length);
  if (containerId.length === 0) return;

  const which = await editor.spawnHostProcess("which", ["docker"]);
  if (which.exit_code !== 0) {
    editor.debug(`devcontainer: docker not on PATH; skipping shutdownAction=stopContainer`);
    return;
  }
  const result = await editor.spawnHostProcess("docker", ["stop", containerId]);
  if (result.exit_code !== 0) {
    editor.debug(
      `devcontainer: docker stop ${containerId} exited ${result.exit_code}: ${result.stderr.trim()}`,
    );
  }
}

/// Abort an in-flight attach by killing the `devcontainer up` host
/// spawn. No-op when nothing is in flight. The indicator is flipped
/// back to Local immediately — cancel is a user-initiated revert,
/// not a failure, so we don't go through FailedAttach.
async function devcontainer_cancel_attach(): Promise<void> {
  const handle = attachInFlight;
  if (!handle) {
    editor.setStatus(editor.t("status.cancel_nothing_in_flight"));
    return;
  }
  // Order matters: set the flag before kill() so the awaiting
  // runDevcontainerUp sees `attachCancelled = true` when the
  // terminal event arrives, and takes the silent-return path
  // instead of painting FailedAttach on top of the Local we're
  // about to install.
  attachCancelled = true;
  editor.setRemoteIndicatorState({ kind: "local" });
  editor.setStatus(editor.t("status.attach_cancelled"));
  // `.kill()` returns a Promise<boolean> from the TS wrapper — we
  // don't need the boolean; the kill is fire-and-forget.
  void handle.kill();
}
registerHandler("devcontainer_cancel_attach", devcontainer_cancel_attach);

/// Open the build log from the most recent `devcontainer up` in a
/// buffer. The path was remembered across restarts via
/// `setGlobalState`, so this works both during Connecting (log is
/// still being appended — Fresh's auto-revert shows live updates)
/// and after a FailedAttach / successful attach.
async function devcontainer_show_build_logs(): Promise<void> {
  const path = readLastBuildLogPath();
  if (!path) {
    editor.setStatus(editor.t("status.no_build_log"));
    return;
  }
  if (editor.readFile(path) === null) {
    editor.setStatus(editor.t("status.build_log_missing"));
    return;
  }
  openBuildLogInSplit(path);
}
registerHandler("devcontainer_show_build_logs", devcontainer_show_build_logs);

/// Show a one-shot snapshot of the attached container's stdout/stderr
/// via `docker logs --tail 1000 <id>`. The log is rendered into a
/// read-only virtual buffer split; closing the split discards the
/// snapshot (re-run the command for a refresh).
///
/// Host-side by design: we talk to the `docker` CLI from outside the
/// container so this works even when the container is mid-reboot or
/// has no shell. The container id comes from the active authority's
/// display label ("Container:<shortid>") rather than re-parsing the
/// `devcontainer up` JSON — plugins own the authority surface, core
/// owns the label.
async function devcontainer_show_logs(): Promise<void> {
  const authorityLabel = editor.getAuthorityLabel();
  const prefix = "Container:";
  if (!authorityLabel.startsWith(prefix)) {
    editor.setStatus(editor.t("status.logs_require_container"));
    return;
  }
  const containerId = authorityLabel.slice(prefix.length);
  if (containerId.length === 0) {
    editor.setStatus(editor.t("status.logs_require_container"));
    return;
  }

  const which = await editor.spawnHostProcess("which", ["docker"]);
  if (which.exit_code !== 0) {
    editor.setStatus(editor.t("status.logs_docker_missing"));
    return;
  }

  editor.setStatus(editor.t("status.logs_loading"));
  const res = await editor.spawnHostProcess(
    "docker",
    ["logs", "--tail", "1000", containerId],
    editor.getCwd(),
  );

  // `docker logs` emits container stdout on our stdout and container
  // stderr on our stderr — merge them with a leading marker so the
  // user can tell them apart in the buffer.
  const mergedParts: string[] = [];
  if (res.stdout.length > 0) {
    mergedParts.push(res.stdout);
  }
  if (res.stderr.length > 0) {
    mergedParts.push("--- stderr ---\n" + res.stderr);
  }
  const merged = mergedParts.join("\n").length > 0
    ? mergedParts.join("\n")
    : editor.t("status.logs_empty");

  // Bug #6 retest: was always splitting on every invocation
  // (no dedupe at all). Route through the shared panel slot.
  const result = await openVirtualInPanelSlot({
    name: "*Dev Container Logs*",
    mode: "devcontainer-info",
    entries: [{ text: merged, properties: { type: "log" } }],
  });
  if (result !== null) {
    editor.setStatus(editor.t("status.logs_shown"));
  }
}
registerHandler("devcontainer_show_logs", devcontainer_show_logs);

// =============================================================================
// Scaffold
// =============================================================================

/// Write a minimal `.devcontainer/devcontainer.json` when the workspace
/// doesn't have one yet, and open it for editing. The template is
/// deliberately conservative — the user picks an image and tweaks
/// lifecycle hooks from there. Matches the spec's "Configure Dev
/// Container" entry for the Local branch of the Remote Indicator
/// popup.
function devcontainer_scaffold_config(): void {
  const cwd = editor.getCwd();
  const dcDir = editor.pathJoin(cwd, ".devcontainer");
  const configFile = editor.pathJoin(dcDir, "devcontainer.json");

  // Respect an existing config — always a safer default than
  // overwriting. The user can call `devcontainer_open_config` if they
  // just meant to edit it.
  if (editor.fileExists(configFile)) {
    editor.setStatus(editor.t("status.scaffold_already_exists"));
    editor.openFile(configFile, null, null);
    return;
  }

  if (!editor.createDir(dcDir)) {
    editor.setStatus(editor.t("status.scaffold_failed"));
    return;
  }

  const workspaceName = cwd.split("/").filter(Boolean).pop() ?? "workspace";
  const template =
    JSON.stringify(
      {
        name: workspaceName,
        image: "mcr.microsoft.com/devcontainers/base:ubuntu",
      },
      null,
      2,
    ) + "\n";

  if (!editor.writeFile(configFile, template)) {
    editor.setStatus(editor.t("status.scaffold_failed"));
    return;
  }

  // Refresh the in-memory config so a subsequent "Reopen in Container"
  // uses the new file without requiring a plugin reload.
  try {
    config = editor.parseJsonc(template) as DevContainerConfig;
    configPath = configFile;
    registerCommands();
  } catch (e) {
    editor.debug("devcontainer: scaffold parse failed: " + String(e));
  }

  editor.setStatus(editor.t("status.scaffold_created"));
  editor.openFile(configFile, null, null);
}
registerHandler("devcontainer_scaffold_config", devcontainer_scaffold_config);

// =============================================================================
// One-shot attach prompt
// =============================================================================
//
// When the plugin loads and a devcontainer.json is found, check whether
// we've already asked the user about this workspace. If not, surface a
// one-shot "attach?" popup. The answer is remembered per-workspace via
// plugin global state (keyed by cwd) so reopening the same project
// doesn't re-prompt every time.

type AttachDecision = "attached" | "dismissed";

function attachDecisionKey(): string {
  return "attach:" + editor.getCwd();
}

function readAttachDecision(): AttachDecision | null {
  const raw = editor.getGlobalState(attachDecisionKey()) as unknown;
  if (raw === "attached" || raw === "dismissed") return raw;
  return null;
}

function writeAttachDecision(value: AttachDecision): void {
  editor.setGlobalState(attachDecisionKey(), value);
}

/// In-memory "Ignore (once)" — true after the user picks the
/// session-only Ignore option in the attach popup. Cleared on
/// plugin reload, which means the next editor restart asks again.
/// This is deliberately separate from the persisted "Ignore (always)"
/// decision (`writeAttachDecision("dismissed")`) so users have a
/// real choice between "not now" and "stop asking forever".
let attachDismissedThisSession = false;

/// Breadcrumb written before calling `editor.setAuthority(payload)`
/// — setAuthority restarts the editor, so there's no clean callback
/// to hook once the new authority is live. If the post-restart plugin
/// instance sees this key with no matching container authority
/// installed, the attach round-tripped through setAuthority but the
/// core failed to construct the authority (rare: a rejected
/// AuthorityPayload). We surface that as FailedAttach so users aren't
/// stuck wondering why Connecting silently became Local.
///
/// The key carries the epoch-ms timestamp of the attempt so stale
/// entries from long-dormant sessions don't bleed into a fresh
/// attach years later.
function attachAttemptKey(): string {
  return "attach-attempt:" + editor.getCwd();
}

function writeAttachAttempt(): void {
  editor.setGlobalState(attachAttemptKey(), String(Date.now()));
}

function clearAttachAttempt(): void {
  editor.setGlobalState(attachAttemptKey(), null);
}

function readAttachAttemptMs(): number | null {
  const raw = editor.getGlobalState(attachAttemptKey()) as unknown;
  if (typeof raw === "string") {
    const n = Number(raw);
    return Number.isFinite(n) ? n : null;
  }
  return null;
}

function showAttachPrompt(): void {
  editor.showActionPopup({
    id: "devcontainer-attach",
    title: editor.t("popup.attach_title"),
    message: editor.t("popup.attach_message", {
      name: config?.name ?? "unnamed",
    }),
    actions: [
      { id: "attach", label: editor.t("popup.attach_action_attach") },
      { id: "dismiss_once", label: editor.t("popup.attach_action_dismiss_once") },
      { id: "dismiss_always", label: editor.t("popup.attach_action_dismiss_always") },
    ],
  });
}

function devcontainer_on_attach_popup(data: ActionPopupResultData): void {
  if (data.popup_id !== "devcontainer-attach") return;
  if (data.action_id === "attach") {
    writeAttachDecision("attached");
    // Fire and forget: runDevcontainerUp's setAuthority call restarts
    // the editor, so nothing after this runs anyway.
    void devcontainer_attach();
  } else if (data.action_id === "dismiss_always") {
    // Persistent ignore: write to plugin global state so the next
    // editor restart in this workspace finds the breadcrumb and
    // skips the popup entirely.
    writeAttachDecision("dismissed");
  } else {
    // `dismiss_once` (or the legacy `dismiss` id from older
    // popups whose state is replayed mid-session): in-memory flag
    // only. The next editor restart in this workspace re-asks.
    attachDismissedThisSession = true;
  }
}
registerHandler("devcontainer_on_attach_popup", devcontainer_on_attach_popup);

// =============================================================================
// Event Handlers
// =============================================================================

editor.on("prompt_confirmed", "devcontainer_on_lifecycle_confirmed");
editor.on("action_popup_result", "devcontainer_on_action_result");
editor.on("authority_changed", "devcontainer_on_authority_changed");

/// Re-register state-gated commands when the authority transitions
/// (local ↔ container). Without this, after `setAuthority` lands a
/// container we'd still have `Attach` / `Cancel Startup` in the
/// palette and `Detach` / `Show Logs` missing.
///
/// Also runs the auto-forward port-detection sweep when entering
/// container mode — Bug #4 (L171). Detecting an entry from
/// `forwardPorts` that's actually bound (host-side) and emitting
/// the spec'd `onAutoForward: notify` toast.
function devcontainer_on_authority_changed(data: unknown): void {
  registerCommands();
  const label = (data as { label?: string } | undefined)?.label ?? "";
  if (label.startsWith("Container:")) {
    void runAutoForwardSweep();
  } else {
    notifiedPorts.clear();
  }
}
registerHandler("devcontainer_on_authority_changed", devcontainer_on_authority_changed);

/// Set of `port/protocol` keys we've already fired the
/// `onAutoForward: notify` toast for in the current attach
/// session. Cleared on detach so a re-attach re-notifies.
const notifiedPorts = new Set<string>();

/// Bug #4 (L171): emit the spec'd `onAutoForward: notify` toast
/// for ports that are both declared in `forwardPorts` AND
/// actually bound on the host (visible in `docker port <id>`).
///
/// Scoped fix: only the notification half of the spec. Actually
/// publishing ports that aren't already mapped is a separate
/// effort — it requires either a host-side userspace forwarder
/// (the VS Code approach) or `appPort`/runArgs glue when starting
/// the container, both of which are larger than this commit.
async function runAutoForwardSweep(): Promise<void> {
  if (!config?.forwardPorts || config.forwardPorts.length === 0) return;
  const rows = await gatherForwardedPortRows();
  for (const row of rows) {
    if (row.source !== "configured") continue;
    if (!row.binding) continue;
    const attrs = config.portsAttributes?.[row.port];
    if (attrs?.onAutoForward !== "notify") continue;
    const key = `${row.port}/${row.protocol.toLowerCase()}`;
    if (notifiedPorts.has(key)) continue;
    notifiedPorts.add(key);
    const labelSuffix = attrs.label ? ` (${attrs.label})` : "";
    editor.setStatus(
      editor.t("status.port_forwarded", {
        port: row.port,
        label: labelSuffix,
      }),
    );
  }
}

// =============================================================================
// Command Registration
// =============================================================================

/// State-gated commands that get re-evaluated on every authority
/// transition. Listed in one place so `registerCommands` and the
/// `authority_changed` cleanup path stay in sync.
///
/// `show_forwarded_ports_panel` stays available in BOTH modes —
/// the panel renders configured `forwardPorts` even when no
/// container is up, which is useful for previewing a config
/// (and one of the tests exercises that exact "configured only"
/// branch).
const ATTACHED_ONLY_COMMANDS = ["%cmd.detach", "%cmd.show_logs"];
const DETACHED_ONLY_COMMANDS = ["%cmd.attach", "%cmd.cancel_attach"];

/// Bug #3 (L170): when `devcontainer.json` exists but fails to
/// parse, register *only* a tiny recovery set so the user has an
/// in-editor path to fix the JSON. Without this, the plugin's
/// init path used to register zero commands and the user lost
/// the entire `Dev Container:` family until restarting the
/// editor.
function registerRecoveryCommands(): void {
  // Drop full-mode entries in case we're transitioning from a
  // working config to a broken one (rebuild → restart → reparse
  // fails). `unregisterCommand` is a no-op when the name isn't
  // registered.
  for (const name of ATTACHED_ONLY_COMMANDS) editor.unregisterCommand(name);
  for (const name of DETACHED_ONLY_COMMANDS) editor.unregisterCommand(name);
  for (const name of [
    "%cmd.show_info",
    "%cmd.show_features",
    "%cmd.show_ports",
    "%cmd.rebuild",
    "%cmd.run_lifecycle",
  ]) {
    editor.unregisterCommand(name);
  }
  // `Open Config` is the recovery escape hatch — we set
  // `configPath` even on parse failure so this opens the broken
  // file in the editor for the user to repair.
  editor.registerCommand(
    "%cmd.open_config",
    "%cmd.open_config_desc",
    "devcontainer_open_config",
    null,
  );
  // `Show Build Logs` stays available so the user can read the
  // last rebuild output (likely shows the validation error from
  // the CLI / docker layer too).
  editor.registerCommand(
    "%cmd.show_build_logs",
    "%cmd.show_build_logs_desc",
    "devcontainer_show_build_logs",
    null,
  );
}

function registerCommands(): void {
  // Commands that are state-relevant in BOTH local and container
  // modes (`Show Info`, `Open Config`, etc.) get registered
  // unconditionally. Commands that are only meaningful in one
  // mode are gated below — `attach` / `cancel_attach` only when
  // not already attached, `detach` / `show_forwarded_ports_panel` /
  // `show_logs` only when attached. The plugin reloads after
  // `setAuthority` AND we listen for `authority_changed` so this
  // function runs on every transition.
  const attached = editor.getAuthorityLabel().startsWith("Container:");
  // Drop any stale state-gated registrations from the previous
  // mode before re-registering. `editor.unregisterCommand` is a
  // no-op when the name isn't registered, so this is safe to call
  // even on first init.
  for (const name of ATTACHED_ONLY_COMMANDS) editor.unregisterCommand(name);
  for (const name of DETACHED_ONLY_COMMANDS) editor.unregisterCommand(name);
  editor.registerCommand(
    "%cmd.show_info",
    "%cmd.show_info_desc",
    "devcontainer_show_info",
    null,
  );
  editor.registerCommand(
    "%cmd.open_config",
    "%cmd.open_config_desc",
    "devcontainer_open_config",
    null,
  );
  editor.registerCommand(
    "%cmd.show_features",
    "%cmd.show_features_desc",
    "devcontainer_show_features",
    null,
  );
  editor.registerCommand(
    "%cmd.show_ports",
    "%cmd.show_ports_desc",
    "devcontainer_show_ports",
    null,
  );
  editor.registerCommand(
    "%cmd.rebuild",
    "%cmd.rebuild_desc",
    "devcontainer_rebuild",
    null,
  );
  editor.registerCommand(
    "%cmd.show_build_logs",
    "%cmd.show_build_logs_desc",
    "devcontainer_show_build_logs",
    null,
  );
  // `run_lifecycle` works in both modes — `initializeCommand` is
  // host-side per spec, the rest are container-side. The picker
  // itself filters which entries are runnable.
  editor.registerCommand(
    "%cmd.run_lifecycle",
    "%cmd.run_lifecycle_desc",
    "devcontainer_run_lifecycle",
    null,
  );
  // `show_forwarded_ports_panel` works in both modes too — the
  // panel renders configured `forwardPorts` even with no
  // container up. (The "configured only" branch is what the
  // panel's regression test exercises.)
  editor.registerCommand(
    "%cmd.show_forwarded_ports_panel",
    "%cmd.show_forwarded_ports_panel_desc",
    "devcontainer_show_forwarded_ports_panel",
    null,
  );
  if (attached) {
    editor.registerCommand(
      "%cmd.detach",
      "%cmd.detach_desc",
      "devcontainer_detach",
      null,
    );
    editor.registerCommand(
      "%cmd.show_logs",
      "%cmd.show_logs_desc",
      "devcontainer_show_logs",
      null,
    );
  } else {
    editor.registerCommand(
      "%cmd.attach",
      "%cmd.attach_desc",
      "devcontainer_attach",
      null,
    );
    editor.registerCommand(
      "%cmd.cancel_attach",
      "%cmd.cancel_attach_desc",
      "devcontainer_cancel_attach",
      null,
    );
  }
}

// =============================================================================
// Initialization
// =============================================================================

// The scaffold command is the only palette entry that makes sense
// without a detected config — it's how the user creates one. Register
// unconditionally so "Dev Container: Create Config" is reachable from
// a cold workspace.
editor.registerCommand(
  "%cmd.scaffold_config",
  "%cmd.scaffold_config_desc",
  "devcontainer_scaffold_config",
  null,
);

if (findConfig()) {
  registerCommands();

  const name = config!.name ?? "unnamed";
  const image = getImageSummary();
  const featureCount = config!.features ? Object.keys(config!.features).length : 0;
  const portCount = config!.forwardPorts?.length ?? 0;

  editor.setStatus(
    editor.t("status.detected", {
      name,
      image,
      features: String(featureCount),
      ports: String(portCount),
    }),
  );

  editor.debug("Dev Container plugin initialized: " + name);

  // Decide whether to surface the attach prompt AFTER main.rs installs
  // the boot authority. When the plugin's top-level body runs, the
  // editor is still being constructed and `authority.display_label` is
  // whatever the default Authority carried during Editor construction —
  // which is empty even on the post-attach restart, because the real
  // container authority is only installed via `set_boot_authority`
  // (called right before `plugins_loaded` fires). Deferring to this
  // hook means `getAuthorityLabel()` reads the freshly-refreshed
  // snapshot and we don't re-prompt a user who already attached.
  function devcontainer_maybe_show_attach_prompt(): void {
    const authorityLabel = editor.getAuthorityLabel();
    const alreadyAttached = authorityLabel.length > 0;

    // Post-restart recovery: clear or surface a FailedAttach for
    // attempts that round-tripped through setAuthority without
    // landing a container. Stale breadcrumbs (> 30 min) are
    // quietly dropped so an old attempt can't poison a fresh
    // session years later.
    const attemptMs = readAttachAttemptMs();
    if (attemptMs !== null) {
      const ageMs = Date.now() - attemptMs;
      const MAX_AGE_MS = 30 * 60 * 1000;
      if (ageMs > MAX_AGE_MS) {
        clearAttachAttempt();
      } else if (alreadyAttached) {
        // Matching container authority came up — success path.
        clearAttachAttempt();
      } else {
        // No container landed but we just tried. Surface it with the
        // same proactive popup as an in-flight failure so users see
        // Retry / Reopen Locally without having to click the
        // indicator.
        enterFailedAttach(editor.t("indicator.error_restart_recovery"));
        clearAttachAttempt();
        // Do not also show the attach prompt — the failed-attach
        // popup is the right next surface; stacking a second popup
        // on top would bury it.
        return;
      }
    }

    if (alreadyAttached) {
      editor.debug(
        "Dev Container plugin: authority '" + authorityLabel + "' already installed, skipping attach prompt",
      );
      return;
    }
    // Persistent dismissal (`Ignore (always in this directory)`)
    // OR a successful prior attach — both are recorded in plugin
    // global state and survive editor restarts. Skip the popup.
    const previousDecision = readAttachDecision();
    if (previousDecision !== null) return;
    // Session-only dismissal (`Ignore (once)`): in-memory flag,
    // cleared on plugin reload so the *next* editor restart
    // re-asks in this workspace.
    if (attachDismissedThisSession) return;
    showAttachPrompt();
  }
  registerHandler(
    "devcontainer_maybe_show_attach_prompt",
    devcontainer_maybe_show_attach_prompt,
  );
  editor.on("plugins_loaded", "devcontainer_maybe_show_attach_prompt");
} else {
  // Bug #2 + #3: a `devcontainer.json` that exists but fails to
  // parse used to fail silently AND drop every `Dev Container:`
  // command from the palette, leaving no in-editor recovery path
  // (the user had to restart the editor). Now: surface the parse
  // error in the status bar AND register a small recovery set
  // (Open Config + Show Build Logs) so the user can navigate to
  // the broken file and fix it.
  if (lastParseError) {
    showParseErrorIfAny();
    registerRecoveryCommands();
  } else {
    editor.debug("Dev Container plugin: no devcontainer.json found");
  }
}
