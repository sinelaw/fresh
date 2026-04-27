/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * YAML LSP Helper Plugin
 *
 * Provides user-friendly error handling for YAML LSP server issues.
 * When yaml-language-server fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects YAML LSP server errors (yaml-language-server)
 * - Shows popup with install commands (npm, yarn, pnpm)
 * - Allows copying install commands to clipboard
 * - Provides option to disable YAML LSP
 *
 * Notes:
 * - yaml-language-server supports JSON Schema validation via modeline comments
 *   e.g. # yaml-language-server: $schema=https://json.schemastore.org/github-workflow.json
 * - Built-in Kubernetes schema support
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

// Install commands for YAML LSP server (yaml-language-server)
// See: https://github.com/redhat-developer/yaml-language-server
const INSTALL_COMMANDS = {
  npm: "npm i -g yaml-language-server",
  yarn: "yarn global add yaml-language-server",
  pnpm: "pnpm add -g yaml-language-server",
};

// Track error state for YAML LSP
let yamlLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for YAML
 */


// Register hook for LSP server errors
editor.on("lsp_server_error", (data) => {
  // Only handle YAML language errors
  if (data.language !== "yaml") {
    return;
  }

  editor.debug(`yaml-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  yamlLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `YAML LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`YAML LSP error: ${data.message}`);
  }
});

/**
 * Handle status bar click when there's a YAML LSP error
 */


// Register hook for status bar clicks
editor.on("lsp_status_clicked", (data) => {
  // Only handle YAML language clicks when there's an error
  if (data.language !== "yaml" || !yamlLspError) {
    return;
  }

  editor.debug("yaml-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "yaml-lsp-help",
    title: "YAML Language Server Not Found",
    message: `"${yamlLspError.serverCommand}" provides code completion, validation, and schema support for YAML files. Requires Node.js. Supports JSON Schema validation and built-in Kubernetes schemas. Copy a command below to install it, or visit https://github.com/redhat-developer/yaml-language-server for details.`,
    actions: [
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "copy_yarn", label: `Copy: ${INSTALL_COMMANDS.yarn}` },
      { id: "copy_pnpm", label: `Copy: ${INSTALL_COMMANDS.pnpm}` },
      { id: "disable", label: "Disable YAML LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});

/**
 * Handle action popup results for YAML LSP help
 */


// Register hook for action popup results
editor.on("action_popup_result", (data) => {
  // Only handle our popup
  if (data.popup_id !== "yaml-lsp-help") {
    return;
  }

  editor.debug(`yaml-lsp: Action selected - ${data.action_id}`);

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
      editor.disableLspForLanguage("yaml");
      editor.setStatus("YAML LSP disabled");
      yamlLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`yaml-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("yaml-lsp: Plugin loaded");
