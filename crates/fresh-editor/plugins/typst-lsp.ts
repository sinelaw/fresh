/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Typst LSP Helper Plugin
 *
 * Provides user-friendly error handling for Typst LSP server issues.
 * When tinymist fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects Typst LSP server errors (tinymist)
 * - Shows popup with install commands (cargo, brew, etc.)
 * - Allows copying install commands to clipboard
 * - Provides option to disable Typst LSP
 *
 * Alternatives:
 * - typst-lsp: Original Typst LSP (superseded by tinymist)
 *
 * Notes:
 * - Tinymist also provides PDF preview and export features
 * - Also available as VS Code extension "Tinymist Typst"
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

// Install commands for Typst LSP server (tinymist)
// See: https://github.com/Myriad-Dreamin/tinymist
const INSTALL_COMMANDS = {
  cargo: "cargo install tinymist",
  brew: "brew install tinymist",
  nix: "nix-env -iA nixpkgs.tinymist",
};

// Track error state for Typst LSP
let typstLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for Typst
 */


// Register hook for LSP server errors
editor.on("lsp_server_error", (data) => {
  // Only handle Typst language errors
  if (data.language !== "typst") {
    return;
  }

  editor.debug(`typst-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  typstLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `Typst LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Typst LSP error: ${data.message}`);
  }
});

/**
 * Handle status bar click when there's a Typst LSP error
 */


// Register hook for status bar clicks
editor.on("lsp_status_clicked", (data) => {
  // Only handle Typst language clicks when there's an error
  if (data.language !== "typst" || !typstLspError) {
    return;
  }

  editor.debug("typst-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "typst-lsp-help",
    title: "Typst Language Server Not Found",
    message: `"${typstLspError.serverCommand}" provides code completion, diagnostics, and preview support for Typst documents. Copy a command below to install it, or visit https://github.com/Myriad-Dreamin/tinymist for details and pre-built binaries. Also available as the "Tinymist Typst" VS Code extension.`,
    actions: [
      { id: "copy_cargo", label: `Copy: ${INSTALL_COMMANDS.cargo}` },
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_nix", label: `Copy: ${INSTALL_COMMANDS.nix}` },
      { id: "disable", label: "Disable Typst LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});

/**
 * Handle action popup results for Typst LSP help
 */


// Register hook for action popup results
editor.on("action_popup_result", (data) => {
  // Only handle our popup
  if (data.popup_id !== "typst-lsp-help") {
    return;
  }

  editor.debug(`typst-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_cargo":
      editor.setClipboard(INSTALL_COMMANDS.cargo);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.cargo);
      break;

    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_nix":
      editor.setClipboard(INSTALL_COMMANDS.nix);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.nix);
      break;

    case "disable":
      editor.disableLspForLanguage("typst");
      editor.setStatus("Typst LSP disabled");
      typstLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`typst-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("typst-lsp: Plugin loaded");
