/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * C# Language Server Support Plugin
 *
 * Provides comprehensive C# LSP support including:
 * - Project root detection (finds nearest .csproj/.sln)
 * - Auto-restore NuGet packages when opening C# files
 * - Handle LSP server requests (e.g., workspace/_roslyn_projectNeedsRestore)
 * - User-friendly error handling when csharp-ls fails to start
 * - Installation help popup with commands to install csharp-ls
 */

// ==================== Type Definitions ====================

interface LspServerRequestData {
  language: string;
  method: string;
  server_command: string;
  params: string | null;
}

interface LspServerErrorData {
  language: string;
  server_command: string;
  error_type: string;
  message: string;
}

interface LspStatusClickedData {
  language: string;
  has_error: boolean;
}

interface ActionPopupResultData {
  popup_id: string;
  action_id: string;
}

interface ProjectNeedsRestoreParams {
  projectFilePath: string;
}

interface AfterFileOpenData {
  path: string;
  buffer_id: number;
}

// ==================== State ====================

// Track which directories we've already restored to avoid repeated restores
const restoredDirectories = new Set<string>();

// Track which project roots we've already set for LSP
const configuredProjectRoots = new Set<string>();

// Cache whether dotnet is available (null = not checked yet)
let dotnetAvailable: boolean | null = null;

// Track error state for C# LSP
let csharpLspError: { serverCommand: string; message: string } | null = null;

// Install command for csharp-ls
const INSTALL_COMMAND = "dotnet tool install --global csharp-ls";

// ==================== Utility Functions ====================

/**
 * Check if dotnet CLI is available
 */
async function isDotnetAvailable(): Promise<boolean> {
  if (dotnetAvailable !== null) {
    return dotnetAvailable;
  }

  try {
    const result = await editor.spawnProcess("dotnet", ["--version"]);
    dotnetAvailable = result.exit_code === 0;
  } catch {
    dotnetAvailable = false;
  }

  if (!dotnetAvailable) {
    editor.debug("csharp_support: dotnet CLI not found, C# support will be limited");
  }

  return dotnetAvailable;
}

/**
 * Get the directory containing the file
 */
function getDirectory(filePath: string): string {
  return editor.pathDirname(filePath);
}

/**
 * Find the project root by walking up directories looking for .csproj or .sln files
 * Returns the directory containing the project file, or null if not found
 */
function findProjectRoot(startPath: string): string | null {
  let currentDir = getDirectory(startPath);
  const maxDepth = 20; // Prevent infinite loops
  let depth = 0;

  while (depth < maxDepth) {
    // Check if this directory contains a .csproj or .sln file
    try {
      const entries = editor.readDir(currentDir);
      for (const entry of entries) {
        if (entry.is_file) {
          if (entry.name.endsWith(".csproj") || entry.name.endsWith(".sln")) {
            editor.debug(`csharp_support: Found project file ${entry.name} in ${currentDir}`);
            return currentDir;
          }
        }
      }
    } catch (e) {
      // Directory read failed, stop searching
      editor.debug(`csharp_support: Failed to read directory ${currentDir}: ${e}`);
      break;
    }

    // Move up to parent directory
    const parentDir = editor.pathDirname(currentDir);
    if (parentDir === currentDir || parentDir === "/" || parentDir === "") {
      // Reached root or can't go higher
      break;
    }
    currentDir = parentDir;
    depth++;
  }

  editor.debug(`csharp_support: No .csproj or .sln found for ${startPath}`);
  return null;
}

/**
 * Run dotnet restore for a project
 */
async function restoreProject(projectPath: string): Promise<void> {
  if (!(await isDotnetAvailable())) {
    return;
  }

  editor.setStatus(editor.t("status.restoring_packages", { project: projectPath }));
  editor.debug(`csharp_support: Running dotnet restore for ${projectPath}`);

  try {
    const result = await editor.spawnProcess("dotnet", ["restore", projectPath]);

    if (result.exit_code === 0) {
      editor.setStatus(editor.t("status.restore_completed", { project: projectPath }));
      editor.debug(`csharp_support: dotnet restore succeeded`);
    } else {
      editor.setStatus(editor.t("status.restore_failed", { error: result.stderr }));
      editor.debug(`csharp_support: dotnet restore failed: ${result.stderr}`);
    }
  } catch (e) {
    const err = e instanceof Error ? e : new Error(String(e));
    editor.setStatus(editor.t("status.restore_error", { error: err.message }));
    editor.debug(`csharp_support: dotnet restore error: ${err.message}`);
  }
}

// ==================== Event Handlers ====================

/**
 * Handle file open - set project root and restore packages
 */
globalThis.on_csharp_file_open = async function (data: AfterFileOpenData): Promise<void> {
  // Only handle .cs files
  if (!data.path.endsWith(".cs")) {
    return;
  }

  editor.debug(`csharp_support: C# file opened: ${data.path}`);

  // Find the project root
  const projectRoot = findProjectRoot(data.path);

  if (projectRoot) {
    // Set the LSP root URI if we haven't already for this project
    if (!configuredProjectRoots.has(projectRoot)) {
      configuredProjectRoots.add(projectRoot);

      // Convert path to file:// URI
      const rootUri = `file://${projectRoot}`;
      editor.debug(`csharp_support: Setting LSP root URI to ${rootUri}`);
      editor.setLspRootUri("csharp", rootUri);
    }

    // Run dotnet restore if we haven't already for this directory
    if (!restoredDirectories.has(projectRoot)) {
      restoredDirectories.add(projectRoot);
      editor.debug(`csharp_support: Running dotnet restore in ${projectRoot}`);
      await restoreProject(projectRoot);
    }
  } else {
    // No project file found - use file's directory for restore
    const dir = getDirectory(data.path);
    if (!restoredDirectories.has(dir)) {
      restoredDirectories.add(dir);
      editor.debug(`csharp_support: No project found, running dotnet restore in ${dir}`);
      await restoreProject(dir);
    }
  }
};

// Register hook for file open
editor.on("after_file_open", "on_csharp_file_open");

/**
 * Handle LSP server requests from C# language servers (Roslyn-based)
 */
globalThis.on_csharp_lsp_server_request = function (data: LspServerRequestData): void {
  // Only handle requests from C# language servers
  if (data.server_command !== "csharp-ls" && data.server_command !== "csharp-language-server") {
    return;
  }

  editor.debug(`csharp_support: Received LSP request ${data.method} from ${data.server_command}`);

  switch (data.method) {
    case "workspace/_roslyn_projectNeedsRestore": {
      // Roslyn LSP server is asking us to restore a project
      if (data.params) {
        try {
          const params: ProjectNeedsRestoreParams = JSON.parse(data.params);
          if (params.projectFilePath) {
            restoreProject(params.projectFilePath);
          }
        } catch (e) {
          editor.debug(`csharp_support: Failed to parse params: ${e}`);
        }
      }
      break;
    }

    default:
      // Log unhandled requests for debugging
      editor.debug(`csharp_support: Unhandled LSP request: ${data.method}`);
  }
};

// Register hook for LSP server requests
editor.on("lsp_server_request", "on_csharp_lsp_server_request");

/**
 * Handle LSP server errors for C#
 */
globalThis.on_csharp_lsp_server_error = function (data: LspServerErrorData): void {
  // Only handle C# language errors
  if (data.language !== "csharp") {
    return;
  }

  editor.debug(`csharp_support: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  csharpLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `C# LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`C# LSP error: ${data.message}`);
  }
};

// Register hook for LSP server errors
editor.on("lsp_server_error", "on_csharp_lsp_server_error");

/**
 * Handle status bar click when there's a C# LSP error
 */
globalThis.on_csharp_lsp_status_clicked = function (data: LspStatusClickedData): void {
  // Only handle C# language clicks when there's an error
  if (data.language !== "csharp" || !csharpLspError) {
    return;
  }

  editor.debug("csharp_support: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "csharp-lsp-help",
    title: "C# Language Server Not Found",
    message: `"${csharpLspError.serverCommand}" provides code completion, diagnostics, and navigation for C# files. Requires .NET SDK. Copy the command below to install it.`,
    actions: [
      { id: "copy_dotnet", label: `Copy: ${INSTALL_COMMAND}` },
      { id: "disable", label: "Disable C# LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
};

// Register hook for status bar clicks
editor.on("lsp_status_clicked", "on_csharp_lsp_status_clicked");

/**
 * Handle action popup results for C# LSP help
 */
globalThis.on_csharp_lsp_action_result = function (data: ActionPopupResultData): void {
  // Only handle our popup
  if (data.popup_id !== "csharp-lsp-help") {
    return;
  }

  editor.debug(`csharp_support: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_dotnet":
      editor.setClipboard(INSTALL_COMMAND);
      editor.setStatus("Copied: " + INSTALL_COMMAND);
      break;

    case "disable":
      editor.disableLspForLanguage("csharp");
      editor.setStatus("C# LSP disabled");
      csharpLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`csharp_support: Unknown action: ${data.action_id}`);
  }
};

// Register hook for action popup results
editor.on("action_popup_result", "on_csharp_lsp_action_result");

editor.debug("csharp_support: Plugin loaded");
