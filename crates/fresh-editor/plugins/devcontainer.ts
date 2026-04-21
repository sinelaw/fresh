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
  value: string;
}): Promise<void> {
  if (data.prompt_type !== "devcontainer-lifecycle") return;

  const cmdName = data.value;
  if (!config || !cmdName) return;

  const cmd = (config as Record<string, unknown>)[cmdName] as LifecycleCommand | undefined;
  if (!cmd) return;

  if (typeof cmd === "string") {
    editor.setStatus(editor.t("status.running", { name: cmdName }));
    const result = await editor.spawnProcess("sh", ["-c", cmd], editor.getCwd());
    if (result.exit_code === 0) {
      editor.setStatus(editor.t("status.completed", { name: cmdName }));
    } else {
      editor.setStatus(editor.t("status.failed", { name: cmdName, code: String(result.exit_code) }));
    }
  } else if (Array.isArray(cmd)) {
    const [bin, ...args] = cmd;
    editor.setStatus(editor.t("status.running", { name: cmdName }));
    const result = await editor.spawnProcess(bin, args, editor.getCwd());
    if (result.exit_code === 0) {
      editor.setStatus(editor.t("status.completed", { name: cmdName }));
    } else {
      editor.setStatus(editor.t("status.failed", { name: cmdName, code: String(result.exit_code) }));
    }
  } else {
    // Object form: run each named sub-command sequentially
    for (const [label, subcmd] of Object.entries(cmd)) {
      editor.setStatus(editor.t("status.running_sub", { name: cmdName, label }));
      let bin: string;
      let args: string[];
      if (Array.isArray(subcmd)) {
        [bin, ...args] = subcmd;
      } else {
        bin = "sh";
        args = ["-c", subcmd as string];
      }
      const result = await editor.spawnProcess(bin, args, editor.getCwd());
      if (result.exit_code !== 0) {
        editor.setStatus(editor.t("status.failed_sub", { name: cmdName, label, code: String(result.exit_code) }));
        return;
      }
    }
    editor.setStatus(editor.t("status.completed", { name: cmdName }));
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

function devcontainer_show_ports(): void {
  if (!config || !config.forwardPorts || config.forwardPorts.length === 0) {
    editor.setStatus(editor.t("status.no_ports"));
    return;
  }

  const suggestions: PromptSuggestion[] = config.forwardPorts.map((port) => {
    const attrs = config!.portsAttributes?.[String(port)];
    const proto = attrs?.protocol ?? "tcp";
    let desc = proto;
    if (attrs?.label) desc += ` - ${attrs.label}`;
    if (attrs?.onAutoForward) desc += ` (${attrs.onAutoForward})`;
    return { text: String(port), description: desc };
  });

  editor.startPrompt(editor.t("prompt.ports"), "devcontainer-ports");
  editor.setPromptSuggestions(suggestions);
}
registerHandler("devcontainer_show_ports", devcontainer_show_ports);

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
  }
}
registerHandler("devcontainer_on_action_result", devcontainer_on_action_result);

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

  // initializeCommand runs on the host BEFORE `devcontainer up`, per
  // spec. Bail the attach if it fails; the user shouldn't get an
  // attached container after their host-side prologue errored.
  if (!(await runInitializeCommand())) {
    return;
  }

  editor.setStatus(editor.t("status.rebuilding"));
  const args = ["up", "--workspace-folder", cwd, ...extraArgs];
  const result = await editor.spawnHostProcess("devcontainer", args);
  if (result.exit_code !== 0) {
    editor.setStatus(
      editor.t("status.rebuild_failed", { error: result.stderr.trim() || "unknown" }),
    );
    return;
  }

  const parsed = parseDevcontainerUpOutput(result.stdout);
  if (!parsed || parsed.outcome !== "success" || !parsed.containerId) {
    editor.setStatus(
      editor.t("status.rebuild_failed", { error: "could not parse devcontainer up output" }),
    );
    return;
  }

  const payload = buildContainerAuthorityPayload(parsed);
  if (!payload) {
    editor.setStatus(
      editor.t("status.rebuild_failed", { error: "missing containerId" }),
    );
    return;
  }

  // setAuthority fires the restart flow in core. The status message
  // we set here won't survive the restart; the plugin will re-init
  // with the new authority active and print status.detected again.
  editor.setAuthority(payload);
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

async function devcontainer_detach(): Promise<void> {
  editor.clearAuthority();
}
registerHandler("devcontainer_detach", devcontainer_detach);

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
