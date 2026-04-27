/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * PHP LSP Helper Plugin
 *
 * Provides user-friendly error handling for PHP LSP server issues.
 * When phpactor fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects PHP LSP server errors (phpactor)
 * - Shows popup with install commands (composer, brew)
 * - Allows copying install commands to clipboard
 * - Provides option to disable PHP LSP
 *
 * Alternatives:
 * - Intelephense: Feature-rich PHP LSP (https://intelephense.com/) - npm i -g intelephense
 * - PHPStan: Static analysis tool (https://phpstan.org/)
 * - Psalm: PHP static analysis (https://psalm.dev/)
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

// Install commands for PHP LSP server (phpactor)
// See: https://phpactor.readthedocs.io/en/master/usage/getting-started.html
const INSTALL_COMMANDS = {
  composer: "composer global require phpactor/phpactor",
  brew: "brew install phpactor",
};

// Alternative LSP server
const ALT_INSTALL = "npm i -g intelephense";

// Track error state for PHP LSP
let phpLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for PHP
 */


// Register hook for LSP server errors
editor.on("lsp_server_error", (data) => {
  // Only handle PHP language errors
  if (data.language !== "php") {
    return;
  }

  editor.debug(`php-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  phpLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `PHP LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`PHP LSP error: ${data.message}`);
  }
});

/**
 * Handle status bar click when there's a PHP LSP error
 */


// Register hook for status bar clicks
editor.on("lsp_status_clicked", (data) => {
  // Only handle PHP language clicks when there's an error
  if (data.language !== "php" || !phpLspError) {
    return;
  }

  editor.debug("php-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "php-lsp-help",
    title: "PHP Language Server Not Found",
    message: `"${phpLspError.serverCommand}" provides code completion, diagnostics, and navigation for PHP files. Requires PHP and Composer. Copy a command below to install it, or visit https://phpactor.readthedocs.io for details. Alternative: Intelephense (https://intelephense.com/) is a popular Node.js-based PHP LSP.`,
    actions: [
      { id: "copy_composer", label: `Copy: ${INSTALL_COMMANDS.composer}` },
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_alt", label: `Alternative: ${ALT_INSTALL}` },
      { id: "disable", label: "Disable PHP LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});

/**
 * Handle action popup results for PHP LSP help
 */


// Register hook for action popup results
editor.on("action_popup_result", (data) => {
  // Only handle our popup
  if (data.popup_id !== "php-lsp-help") {
    return;
  }

  editor.debug(`php-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_composer":
      editor.setClipboard(INSTALL_COMMANDS.composer);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.composer);
      break;

    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_alt":
      editor.setClipboard(ALT_INSTALL);
      editor.setStatus("Copied: " + ALT_INSTALL);
      break;

    case "disable":
      editor.disableLspForLanguage("php");
      editor.setStatus("PHP LSP disabled");
      phpLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`php-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("php-lsp: Plugin loaded");
