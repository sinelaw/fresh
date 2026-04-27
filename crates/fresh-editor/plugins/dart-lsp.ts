/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Dart LSP Helper Plugin
 *
 * Provides user-friendly error handling for Dart LSP server issues.
 * The Dart language server is included with the Dart SDK.
 *
 * Features:
 * - Detects Dart LSP server errors
 * - Shows popup with install instructions for Dart SDK
 * - Provides option to disable Dart LSP
 *
 * VS Code: "Dart" extension by Dart Code (uses bundled analysis server)
 * Neovim: nvim-lspconfig dartls
 * Note: The analysis server is bundled with Dart SDK and Flutter SDK
 * For Flutter: Install Flutter SDK which includes Dart
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

// Install commands for Dart SDK (which includes the language server)
// See: https://dart.dev/get-dart
const INSTALL_COMMANDS = {
  brew: "brew install dart",
  apt: "sudo apt install dart",
  choco: "choco install dart-sdk",
};

// Track error state for Dart LSP
let dartLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for Dart
 */

editor.on("lsp_server_error", (data) => {
  if (data.language !== "dart") {
    return;
  }

  editor.debug(`dart-lsp: Server error - ${data.error_type}: ${data.message}`);

  dartLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Dart LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Dart LSP error: ${data.message}`);
  }
});

/**
 * Handle status bar click when there's a Dart LSP error
 */

editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "dart" || !dartLspError) {
    return;
  }

  editor.debug("dart-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "dart-lsp-help",
    title: "Dart Language Server Not Found",
    message: `The Dart language server is included with the Dart SDK. Install the Dart SDK (or Flutter SDK) to get LSP support. Visit https://dart.dev/get-dart for platform-specific instructions.`,
    actions: [
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_apt", label: `Copy: ${INSTALL_COMMANDS.apt}` },
      { id: "copy_choco", label: `Copy: ${INSTALL_COMMANDS.choco} (Windows)` },
      { id: "disable", label: "Disable Dart LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});

/**
 * Handle action popup results for Dart LSP help
 */

editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "dart-lsp-help") {
    return;
  }

  editor.debug(`dart-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_apt":
      editor.setClipboard(INSTALL_COMMANDS.apt);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.apt);
      break;

    case "copy_choco":
      editor.setClipboard(INSTALL_COMMANDS.choco);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.choco);
      break;

    case "disable":
      editor.disableLspForLanguage("dart");
      editor.setStatus("Dart LSP disabled");
      dartLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`dart-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("dart-lsp: Plugin loaded");
