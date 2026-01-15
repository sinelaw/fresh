/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * Clangd helper plugin
 *
 * Provides two commands:
 *  - Switch Source/Header (uses clangd/textDocument/switchSourceHeader)
 *  - Open project .clangd configuration file
 */

import { PanelManager } from "./lib/index.ts";

const languageMap: Record<string, string> = {
  c: "cpp",
  h: "cpp",
  hp: "cpp",
  hpp: "cpp",
  hxx: "cpp",
  hh: "cpp",
  cpp: "cpp",
  cxx: "cpp",
  cc: "cpp",
  objc: "cpp",
  mm: "cpp",
};

function detectLanguage(path: string): string | null {
  const segments = path.split(".");
  if (segments.length === 1) {
    return null;
  }
  const ext = segments[segments.length - 1].toLowerCase();
  return languageMap[ext] ?? null;
}

function pathToFileUri(path: string): string {
  let normalized = path.replace(/\\/g, "/");
  if (!normalized.startsWith("/")) {
    normalized = "/" + normalized;
  }
  return "file://" + encodeURI(normalized);
}

function fileUriToPath(uri: string): string {
  if (!uri.startsWith("file://")) {
    return uri;
  }
  let path = decodeURI(uri.substring("file://".length));
  if (path.startsWith("/") && path.length > 2 && path[2] === ":") {
    path = path.substring(1);
  }
  return path;
}

function setClangdStatus(message: string): void {
  editor.setStatus(message);
}

globalThis.clangdSwitchSourceHeader = async function(): Promise<void> {
  const bufferId = editor.getActiveBufferId();
  const path = editor.getBufferPath(bufferId);
  if (!path) {
    setClangdStatus(editor.t("status.no_active_file"));
    return;
  }

  const language = detectLanguage(path);
  if (!language) {
    setClangdStatus(editor.t("status.unsupported_file_type"));
    return;
  }

  const uri = pathToFileUri(path);
  editor.debug(`clangdSwitchSourceHeader: sending request for ${uri}`);
  try {
    const result = await editor.sendLspRequest(language, "textDocument/switchSourceHeader", {
      textDocument: { uri },
    });
    editor.debug(`clangdSwitchSourceHeader: got result ${JSON.stringify(result)}`);
    if (typeof result === "string" && result.length > 0) {
      const targetPath = fileUriToPath(result);
      editor.openFile(targetPath, 0, 0);
      setClangdStatus(editor.t("status.opened_corresponding_file"));
      return;
    }
    setClangdStatus(editor.t("status.no_matching_found"));
  } catch (err) {
    setClangdStatus(editor.t("status.switch_failed", { error: String(err) }));
    editor.debug(`clangdSwitchSourceHeader error: ${err}`);
  }
};

const projectPanel = new PanelManager(editor, "Clangd project setup", "clangd-project-setup");

function pathDesc(path: string): string {
  if (!path) {
    return "unknown";
  }
  return path;
}

function detectProjectRoot(): string | null {
  const roots = new Set<string>();
  const cwd = editor.getCwd();
  if (cwd) {
    roots.add(cwd);
  }
  const bufferId = editor.getActiveBufferId();
  const bufferPath = editor.getBufferPath(bufferId);
  if (bufferPath) {
    // Add the buffer directory and a few ancestors
    let current = editor.pathDirname(bufferPath);
    let depth = 0;
    while (current && depth < 5) {
      roots.add(current);
      current = editor.pathDirname(current);
      depth += 1;
    }
  }
  if (roots.size === 0) {
    return null;
  }
  return Array.from(roots)[0];
}

function analyzeProject(root: string | null) {
  const lines: string[] = [];
  const dirCandidates: string[] = [];
  if (root) {
    dirCandidates.push(root);
  }
  const currentDir = editor.getCwd();
  if (currentDir && currentDir !== root) {
    dirCandidates.push(currentDir);
  }
  const activeBufferId = editor.getActiveBufferId();
  const bufferPath = editor.getBufferPath(activeBufferId);
  if (bufferPath) {
    const bufferDir = editor.pathDirname(bufferPath);
    if (bufferDir && !dirCandidates.includes(bufferDir)) {
      dirCandidates.push(bufferDir);
    }
  }

  const compileCandidates = [
    "compile_commands.json",
    "build/compile_commands.json",
    "out/compile_commands.json",
    "cmake-build-debug/compile_commands.json",
  ];
  let compilePath: string | null = null;
  for (const base of dirCandidates) {
    for (const relative of compileCandidates) {
      const candidate = editor.pathJoin(base, relative);
      if (editor.fileExists(candidate)) {
        compilePath = candidate;
        break;
      }
    }
    if (compilePath) {
      break;
    }
  }

  const clangdFileCandidates = [".clangd", ".clangd/compile_flags.txt"];
  let clangdPath: string | null = null;
  for (const base of dirCandidates) {
    for (const relative of clangdFileCandidates) {
      const candidate = editor.pathJoin(base, relative);
      if (editor.fileExists(candidate)) {
        clangdPath = candidate;
        break;
      }
    }
    if (clangdPath) {
      break;
    }
  }

  const status: string[] = [];
  if (root) {
    status.push(`Project root: ${root}`);
  } else {
    status.push("Project root: (unknown)");
  }
  status.push("");

  if (compilePath) {
    status.push(`Compile commands: ready (${compilePath})`);
  } else {
    status.push("Compile commands: missing");
    status.push("  Tip: run `cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON` or `bear -- make` to generate compile_commands.json and place it at the project root.");
  }

  if (clangdPath) {
    status.push(`.clangd configuration: ${clangdPath}`);
  } else {
    status.push(".clangd configuration: missing");
    status.push("  Tip: create a .clangd file to customize clangd flags, fallback file, etc.");
  }

  const buildHints: string[] = [];
  if (root) {
    if (editor.fileExists(editor.pathJoin(root, "CMakeLists.txt"))) {
      buildHints.push("CMake project detected (configure with `cmake -S . -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=ON`).");
    }
    if (
      editor.fileExists(editor.pathJoin(root, "WORKSPACE")) ||
      editor.fileExists(editor.pathJoin(root, "WORKSPACE.bazel")) ||
      editor.fileExists(editor.pathJoin(root, "BUILD.bazel"))
    ) {
      buildHints.push("Bazel project detected (use `bazel build //...` and attach the generated compile_commands.json).");
    }
    if (editor.fileExists(editor.pathJoin(root, "Makefile"))) {
      buildHints.push("Makefile detected (run `bear -- make` or `intercept-build make` to emit compile_commands.json).");
    }
  }
  if (buildHints.length > 0) {
    status.push("");
    status.push("Build system hints:");
    for (const hint of buildHints) {
      status.push(`  - ${hint}`);
    }
  }

  status.push("");
  status.push("General tips:");
  status.push("  * Place compile_commands.json at the project root or point clangd to a custom path.");
  status.push("  * Use `Clangd: Open Project Config` to edit project-specific overrides.");
  status.push("  * Use `Clangd: Switch Source/Header` once compile data is available.");

  return status;
}

globalThis.clangdProjectSetup = async function (): Promise<void> {
  const projectRoot = detectProjectRoot();
  const summary = analyzeProject(projectRoot);
  const entries = summary.map((line) => ({
    text: line + "\n",
    properties: {},
  }));
  await projectPanel.open({
    entries,
    ratio: 0.3,
  });
};

editor.registerCommand(
  "%cmd.project_setup",
  "%cmd.project_setup_desc",
  "clangdProjectSetup",
  ""
);

globalThis.clangdOpenProjectConfig = function(): void {
  const bufferId = editor.getActiveBufferId();
  const targets = new Set<string>();
  const bufferPath = editor.getBufferPath(bufferId);
  if (bufferPath) {
    const dir = editor.pathDirname(bufferPath);
    targets.add(dir);
  }
  const cwd = editor.getCwd();
  if (cwd) {
    targets.add(cwd);
  }

  let opened = false;
  for (const dir of Array.from(targets)) {
    const configPath = editor.pathJoin(dir, ".clangd");
    if (editor.fileExists(configPath)) {
      editor.openFile(configPath, 0, 0);
      setClangdStatus(editor.t("status.opened_config"));
      opened = true;
      break;
    }
  }

  if (!opened) {
    setClangdStatus(editor.t("status.config_not_found"));
  }
};

editor.registerCommand(
  "%cmd.switch_source_header",
  "%cmd.switch_source_header_desc",
  "clangdSwitchSourceHeader",
  "normal"
);

editor.registerCommand(
  "%cmd.open_project_config",
  "%cmd.open_project_config_desc",
  "clangdOpenProjectConfig",
  "normal"
);

setClangdStatus(editor.t("status.plugin_loaded"));

globalThis.onClangdCustomNotification = function(payload: {
  language: string;
  method: string;
  params: Record<string, unknown> | null;
}): void {
  if (!payload || payload.language !== "cpp") {
    return;
  }

  editor.debug(
    `clangd notification ${payload.method}: ${JSON.stringify(payload.params)}`,
  );

  if (payload.method === "textDocument/clangd.fileStatus" && payload.params) {
    const status = (payload.params as any).status ?? "unknown";
    editor.debug(`Clangd file status: ${JSON.stringify(status)}`);
    setClangdStatus(editor.t("status.file_status", { status: String(status) }));
  } else if (payload.method === "$/memoryUsage" && payload.params) {
    const usage = (payload.params as any).used ?? "unknown";
    editor.debug(`Clangd memory usage: ${usage}`);
  }
};

editor.on("lsp/custom_notification", "onClangdCustomNotification");
