/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Lua LSP Helper Plugin
 *
 * Provides user-friendly error handling for Lua LSP server issues.
 * When lua-language-server fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects Lua LSP server errors (lua-language-server / LuaLS)
 * - Shows popup with install commands (brew, pacman, etc.)
 * - Allows copying install commands to clipboard
 * - Provides option to disable Lua LSP
 *
 * Alternatives:
 * - EmmyLua: IntelliJ-based Lua IDE support
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

// Install commands for Lua LSP server (lua-language-server / LuaLS)
// See: https://luals.github.io/#install
const INSTALL_COMMANDS = {
  brew: "brew install lua-language-server",
  pacman: "sudo pacman -S lua-language-server",
  nix: "nix-env -i lua-language-server",
};

// Track error state for Lua LSP
let luaLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for Lua
 */


// Register hook for LSP server errors
editor.on("lsp_server_error", (data) => {
  // Only handle Lua language errors
  if (data.language !== "lua") {
    return;
  }

  editor.debug(`lua-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  luaLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `Lua LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Lua LSP error: ${data.message}`);
  }
});

/**
 * Handle status bar click when there's a Lua LSP error
 */


// Register hook for status bar clicks
editor.on("lsp_status_clicked", (data) => {
  // Only handle Lua language clicks when there's an error
  if (data.language !== "lua" || !luaLspError) {
    return;
  }

  editor.debug("lua-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "lua-lsp-help",
    title: "Lua Language Server Not Found",
    message: `"${luaLspError.serverCommand}" (LuaLS) provides code completion, diagnostics, and navigation for Lua files. Copy a command below to install it, or visit https://luals.github.io/#install for platform-specific instructions. Pre-built binaries are also available from https://github.com/LuaLS/lua-language-server/releases.`,
    actions: [
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_pacman", label: `Copy: ${INSTALL_COMMANDS.pacman}` },
      { id: "copy_nix", label: `Copy: ${INSTALL_COMMANDS.nix}` },
      { id: "disable", label: "Disable Lua LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});

/**
 * Handle action popup results for Lua LSP help
 */


// Register hook for action popup results
editor.on("action_popup_result", (data) => {
  // Only handle our popup
  if (data.popup_id !== "lua-lsp-help") {
    return;
  }

  editor.debug(`lua-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_pacman":
      editor.setClipboard(INSTALL_COMMANDS.pacman);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.pacman);
      break;

    case "copy_nix":
      editor.setClipboard(INSTALL_COMMANDS.nix);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.nix);
      break;

    case "disable":
      editor.disableLspForLanguage("lua");
      editor.setStatus("Lua LSP disabled");
      luaLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`lua-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("lua-lsp: Plugin loaded");
