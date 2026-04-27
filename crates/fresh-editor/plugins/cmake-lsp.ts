/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * CMake LSP Helper Plugin
 *
 * Provides user-friendly error handling for CMake LSP server issues.
 * When cmake-language-server fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects CMake LSP server errors (cmake-language-server)
 * - Shows popup with install commands (pip, pipx)
 * - Provides option to disable CMake LSP
 *
 * VS Code: CMake Tools extension (v1.20+ has basic built-in language services)
 * Neovim: nvim-lspconfig cmake
 * Alternative: neocmakelsp (more actively maintained, has linting/formatting)
 * See: https://github.com/neocmakelsp/neocmakelsp
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

// Install commands for CMake LSP server
// See: https://github.com/regen100/cmake-language-server
const INSTALL_COMMANDS = {
  pip: "pip install cmake-language-server",
  pipx: "pipx install cmake-language-server",
};

// Track error state for CMake LSP
let cmakeLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for CMake
 */

editor.on("lsp_server_error", (data) => {
  if (data.language !== "cmake") {
    return;
  }

  editor.debug(`cmake-lsp: Server error - ${data.error_type}: ${data.message}`);

  cmakeLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `CMake LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`CMake LSP error: ${data.message}`);
  }
});

/**
 * Handle status bar click when there's a CMake LSP error
 */

editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "cmake" || !cmakeLspError) {
    return;
  }

  editor.debug("cmake-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "cmake-lsp-help",
    title: "CMake Language Server Not Found",
    message: `"${cmakeLspError.serverCommand}" provides code completion, diagnostics, and navigation for CMakeLists.txt files. Requires Python. Copy a command below to install it, or visit https://github.com/regen100/cmake-language-server for details. Alternative: neocmakelsp (https://github.com/Decodetalkers/neocmakelsp).`,
    actions: [
      { id: "copy_pip", label: `Copy: ${INSTALL_COMMANDS.pip}` },
      { id: "copy_pipx", label: `Copy: ${INSTALL_COMMANDS.pipx}` },
      { id: "disable", label: "Disable CMake LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});

/**
 * Handle action popup results for CMake LSP help
 */

editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "cmake-lsp-help") {
    return;
  }

  editor.debug(`cmake-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_pip":
      editor.setClipboard(INSTALL_COMMANDS.pip);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.pip);
      break;

    case "copy_pipx":
      editor.setClipboard(INSTALL_COMMANDS.pipx);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.pipx);
      break;

    case "disable":
      editor.disableLspForLanguage("cmake");
      editor.setStatus("CMake LSP disabled");
      cmakeLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`cmake-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("cmake-lsp: Plugin loaded");
