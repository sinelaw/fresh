/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * TypeScript/JavaScript LSP Helper Plugin
 *
 * Provides user-friendly error handling for TypeScript/JavaScript LSP server issues.
 * When typescript-language-server fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects TypeScript LSP server errors (typescript-language-server, tsserver)
 * - Shows popup with install commands (npm, yarn, pnpm)
 * - Allows copying install commands to clipboard
 * - Provides option to disable TypeScript LSP
 */

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

// Install commands for TypeScript LSP server
// Both typescript-language-server AND typescript packages are required
// See: https://github.com/typescript-language-server/typescript-language-server
const INSTALL_COMMANDS = {
  npm: "npm install -g typescript-language-server typescript",
  yarn: "yarn global add typescript-language-server typescript",
  pnpm: "pnpm add -g typescript-language-server typescript",
};

// Languages handled by this plugin
const HANDLED_LANGUAGES = ["typescript", "javascript", "typescriptreact", "javascriptreact"];

// Track error state for TypeScript LSP
let tsLspError: { serverCommand: string; message: string; language: string } | null = null;

/**
 * Handle LSP server errors for TypeScript/JavaScript
 */
globalThis.on_typescript_lsp_server_error = function (
  data: LspServerErrorData
): void {
  // Only handle TypeScript/JavaScript language errors
  if (!HANDLED_LANGUAGES.includes(data.language)) {
    return;
  }

  editor.debug(
    `typescript-lsp: Server error - ${data.error_type}: ${data.message}`
  );

  // Store error state for later reference
  tsLspError = {
    serverCommand: data.server_command,
    message: data.message,
    language: data.language,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `TypeScript LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`TypeScript LSP error: ${data.message}`);
  }
};

// Register hook for LSP server errors
editor.on("lsp_server_error", "on_typescript_lsp_server_error");

/**
 * Handle status bar click when there's a TypeScript LSP error
 */
globalThis.on_typescript_lsp_status_clicked = function (
  data: LspStatusClickedData
): void {
  // Only handle TypeScript/JavaScript language clicks when there's an error
  if (!HANDLED_LANGUAGES.includes(data.language) || !tsLspError) {
    return;
  }

  editor.debug("typescript-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "typescript-lsp-help",
    title: "TypeScript Language Server Not Found",
    message: `"${tsLspError.serverCommand}" provides code completion, diagnostics, and navigation for TypeScript/JavaScript files. Copy a command below to install it, or search online for your platform.`,
    actions: [
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "copy_yarn", label: `Copy: ${INSTALL_COMMANDS.yarn}` },
      { id: "copy_pnpm", label: `Copy: ${INSTALL_COMMANDS.pnpm}` },
      { id: "disable", label: "Disable TypeScript LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
};

// Register hook for status bar clicks
editor.on("lsp_status_clicked", "on_typescript_lsp_status_clicked");

/**
 * Handle action popup results for TypeScript LSP help
 */
globalThis.on_typescript_lsp_action_result = function (
  data: ActionPopupResultData
): void {
  // Only handle our popup
  if (data.popup_id !== "typescript-lsp-help") {
    return;
  }

  editor.debug(`typescript-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_npm":
      editor.setClipboard(INSTALL_COMMANDS.npm);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.npm);
      break;

    case "copy_yarn":
      editor.setClipboard(INSTALL_COMMANDS.yarn);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.yarn);
      break;

    case "copy_pnpm":
      editor.setClipboard(INSTALL_COMMANDS.pnpm);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.pnpm);
      break;

    case "disable":
      // Disable for all TypeScript/JavaScript variants
      editor.disableLspForLanguage("typescript");
      editor.disableLspForLanguage("javascript");
      editor.setStatus("TypeScript/JavaScript LSP disabled");
      tsLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`typescript-lsp: Unknown action: ${data.action_id}`);
  }
};

// Register hook for action popup results
editor.on("action_popup_result", "on_typescript_lsp_action_result");

editor.debug("typescript-lsp: Plugin loaded");
