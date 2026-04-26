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

function findConfig(): boolean {
  const cwd = editor.getCwd();

  // Priority 1: .devcontainer/devcontainer.json
  const primary = editor.pathJoin(cwd, ".devcontainer", "devcontainer.json");
  const primaryContent = editor.readFile(primary);
  if (primaryContent !== null) {
    try {
      config = editor.parseJsonc(primaryContent) as DevContainerConfig;
      configPath = primary;
      return true;
    } catch {
      editor.debug("devcontainer: failed to parse " + primary);
    }
  }

  // Priority 2: .devcontainer.json
  const secondary = editor.pathJoin(cwd, ".devcontainer.json");
  const secondaryContent = editor.readFile(secondary);
  if (secondaryContent !== null) {
    try {
      config = editor.parseJsonc(secondaryContent) as DevContainerConfig;
      configPath = secondary;
      return true;
    } catch {
      editor.debug("devcontainer: failed to parse " + secondary);
    }
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
          try {
            config = editor.parseJsonc(subContent) as DevContainerConfig;
            configPath = subConfig;
            return true;
          } catch {
            editor.debug("devcontainer: failed to parse " + subConfig);
          }
        }
      }
    }
  }

  return false;
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

  if (infoPanelOpen && infoPanelBufferId !== null) {
    // Already open - refresh content
    updateInfoPanel();
    return;
  }

  infoFocus = { type: "button", index: 0 };
  const entries = buildInfoEntries();
  cachedContent = entriesToContent(entries);

  const result = await editor.createVirtualBufferInSplit({
    name: "*Dev Container*",
    mode: "devcontainer-info",
    readOnly: true,
    showLineNumbers: false,
    showCursors: true,
    editingDisabled: true,
    lineWrap: true,
    ratio: 0.4,
    direction: "horizontal",
    entries: entries,
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
    if (result.exit_code === 0) {
      editor.setStatus(editor.t("status.completed", { name: cmdName }));
    } else {
      editor.setStatus(editor.t("status.failed", { name: cmdName, code: String(result.exit_code) }));
    }
  } else if (Array.isArray(cmd)) {
    const [origBin, ...origArgs] = cmd;
    const [bin, args] = wrapWithEnv(env, origBin, origArgs);
    editor.setStatus(editor.t("status.running", { name: cmdName }));
    const result = await editor.spawnProcess(bin, args, cwd);
    if (result.exit_code === 0) {
      editor.setStatus(editor.t("status.completed", { name: cmdName }));
    } else {
      editor.setStatus(editor.t("status.failed", { name: cmdName, code: String(result.exit_code) }));
    }
  } else {
    // Object form: see the rewritten parallel branch in
    // `runLifecycleObjectForm`.
    await runLifecycleObjectForm(cmdName, cmd);
  }
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
      if (Array.isArray(subcmd)) {
        [origBin, ...origArgs] = subcmd;
      } else {
        origBin = "sh";
        origArgs = ["-c", subcmd as string];
      }
      const [bin, args] = wrapWithEnv(env, origBin, origArgs);
      const r = await editor.spawnProcess(bin, args, cwd);
      return { label, code: r.exit_code };
    }),
  );

  const failed = results.filter((r) => r.code !== 0);
  if (failed.length === 0) {
    editor.setStatus(editor.t("status.completed", { name: cmdName }));
    return;
  }
  // Surface the first failure in the status message — same key
  // the old sequential path used so existing translations keep
  // working. Other failures are debug-logged so users can see
  // the full picture in the log.
  const first = failed[0];
  editor.setStatus(
    editor.t("status.failed_sub", {
      name: cmdName,
      label: first.label,
      code: String(first.code),
    }),
  );
  for (const f of failed.slice(1)) {
    editor.debug(
      `devcontainer: ${cmdName} (${f.label}) also failed (exit ${f.code})`,
    );
  }
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

  if (portsPanelOpen && portsPanelBufferId !== null) {
    await renderPortsPanel();
    return;
  }

  const rows = await gatherForwardedPortRows();
  const entries = buildPortsPanelEntries(rows);
  const result = await editor.createVirtualBufferInSplit({
    name: "*Dev Container Ports*",
    mode: "devcontainer-ports",
    readOnly: true,
    showLineNumbers: false,
    showCursors: true,
    editingDisabled: true,
    lineWrap: true,
    ratio: 0.35,
    direction: "horizontal",
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

  const payload = buildContainerAuthorityPayload(parsed);
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
function openBuildLogInSplit(path: string): void {
  const buffers = editor.listBuffers();
  const existing = buffers.find((b) => b.path === path);
  if (existing && existing.splits.length > 0) {
    editor.focusSplit(existing.splits[0]);
    return;
  }
  // Not visible anywhere → create a new split and open the log
  // there. openFile reuses the buffer when the path is already
  // loaded (e.g. open but not in any split), so no duplicate
  // buffers either way.
  editor.executeAction("split_horizontal");
  editor.openFile(path, null, null);
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

  const result = await editor.createVirtualBufferInSplit({
    name: "*Dev Container Logs*",
    mode: "devcontainer-info",
    readOnly: true,
    showLineNumbers: false,
    showCursors: true,
    editingDisabled: true,
    lineWrap: true,
    ratio: 0.4,
    direction: "horizontal",
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
      { id: "dismiss", label: editor.t("popup.attach_action_dismiss") },
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
  } else {
    writeAttachDecision("dismissed");
  }
}
registerHandler("devcontainer_on_attach_popup", devcontainer_on_attach_popup);

// =============================================================================
// Event Handlers
// =============================================================================

editor.on("prompt_confirmed", "devcontainer_on_lifecycle_confirmed");
editor.on("action_popup_result", "devcontainer_on_action_result");

// =============================================================================
// Command Registration
// =============================================================================

function registerCommands(): void {
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
    "%cmd.run_lifecycle",
    "%cmd.run_lifecycle_desc",
    "devcontainer_run_lifecycle",
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
    "%cmd.attach",
    "%cmd.attach_desc",
    "devcontainer_attach",
    null,
  );
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
  editor.registerCommand(
    "%cmd.show_build_logs",
    "%cmd.show_build_logs_desc",
    "devcontainer_show_build_logs",
    null,
  );
  editor.registerCommand(
    "%cmd.cancel_attach",
    "%cmd.cancel_attach_desc",
    "devcontainer_cancel_attach",
    null,
  );
  editor.registerCommand(
    "%cmd.show_forwarded_ports_panel",
    "%cmd.show_forwarded_ports_panel_desc",
    "devcontainer_show_forwarded_ports_panel",
    null,
  );
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
    // One-shot per-session dismissal: if the user already said "Not
    // now" in this Editor process, don't re-prompt. On a cold restart
    // the state is gone and we ask again — that's fine.
    const previousDecision = readAttachDecision();
    if (previousDecision !== null) return;
    showAttachPrompt();
  }
  registerHandler(
    "devcontainer_maybe_show_attach_prompt",
    devcontainer_maybe_show_attach_prompt,
  );
  editor.on("plugins_loaded", "devcontainer_maybe_show_attach_prompt");
} else {
  editor.debug("Dev Container plugin: no devcontainer.json found");
}
