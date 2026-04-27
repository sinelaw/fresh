/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Kotlin LSP Helper Plugin
 *
 * Provides user-friendly error handling for Kotlin LSP server issues.
 * When kotlin-language-server fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects Kotlin LSP server errors (kotlin-language-server)
 * - Shows popup with install commands (brew, SDKMAN, snap)
 * - Allows copying install commands to clipboard
 * - Provides option to disable Kotlin LSP
 *
 * VS Code: "Kotlin" extension (fwcd.kotlin)
 * Neovim: nvim-lspconfig kotlin_language_server
 * Note: Requires JDK 11+. For full IDE support, consider IntelliJ IDEA
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

// Install commands for Kotlin LSP server (kotlin-language-server)
// See: https://github.com/fwcd/kotlin-language-server
const INSTALL_COMMANDS = {
  brew: "brew install kotlin-language-server",
  snap: "sudo snap install kotlin-language-server --classic",
  nix: "nix-env -i kotlin-language-server",
};

// Track error state for Kotlin LSP
let kotlinLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for Kotlin
 */


// Register hook for LSP server errors
editor.on("lsp_server_error", (data) => {
  // Only handle Kotlin language errors
  if (data.language !== "kotlin") {
    return;
  }

  editor.debug(`kotlin-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  kotlinLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `Kotlin LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Kotlin LSP error: ${data.message}`);
  }
});

/**
 * Handle status bar click when there's a Kotlin LSP error
 */


// Register hook for status bar clicks
editor.on("lsp_status_clicked", (data) => {
  // Only handle Kotlin language clicks when there's an error
  if (data.language !== "kotlin" || !kotlinLspError) {
    return;
  }

  editor.debug("kotlin-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "kotlin-lsp-help",
    title: "Kotlin Language Server Not Found",
    message: `"${kotlinLspError.serverCommand}" provides code completion, diagnostics, and navigation for Kotlin files. Requires a JDK (Java 11+). Copy a command below to install it, or visit https://github.com/fwcd/kotlin-language-server for build instructions and releases. For full Kotlin IDE support, consider IntelliJ IDEA or Android Studio.`,
    actions: [
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_snap", label: `Copy: ${INSTALL_COMMANDS.snap}` },
      { id: "copy_nix", label: `Copy: ${INSTALL_COMMANDS.nix}` },
      { id: "disable", label: "Disable Kotlin LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});

/**
 * Handle action popup results for Kotlin LSP help
 */


// Register hook for action popup results
editor.on("action_popup_result", (data) => {
  // Only handle our popup
  if (data.popup_id !== "kotlin-lsp-help") {
    return;
  }

  editor.debug(`kotlin-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_snap":
      editor.setClipboard(INSTALL_COMMANDS.snap);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.snap);
      break;

    case "copy_nix":
      editor.setClipboard(INSTALL_COMMANDS.nix);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.nix);
      break;

    case "disable":
      editor.disableLspForLanguage("kotlin");
      editor.setStatus("Kotlin LSP disabled");
      kotlinLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`kotlin-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("kotlin-lsp: Plugin loaded");
