/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Ruby LSP Helper Plugin
 *
 * Provides user-friendly error handling for Ruby LSP server issues.
 * When solargraph fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects Ruby LSP server errors (solargraph)
 * - Shows popup with install commands (gem)
 * - Allows copying install commands to clipboard
 * - Provides option to disable Ruby LSP
 *
 * Alternatives:
 * - ruby-lsp (Shopify): Modern Ruby LSP - gem install ruby-lsp (https://github.com/Shopify/ruby-lsp)
 * - Steep: Ruby type checker with LSP support (https://github.com/soutaro/steep)
 * - Sorbet: Gradual type checker for Ruby (https://sorbet.org/)
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

// Install commands for Ruby LSP server (solargraph)
// See: https://solargraph.org/guides/getting-started
const INSTALL_COMMANDS = {
  gem: "gem install solargraph",
  bundler: "bundle add solargraph --group development",
};

// Alternative LSP server
const ALT_INSTALL = "gem install ruby-lsp";

// Track error state for Ruby LSP
let rubyLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for Ruby
 */


// Register hook for LSP server errors
editor.on("lsp_server_error", (data) => {
  // Only handle Ruby language errors
  if (data.language !== "ruby") {
    return;
  }

  editor.debug(`ruby-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference
  rubyLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `Ruby LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Ruby LSP error: ${data.message}`);
  }
});

/**
 * Handle status bar click when there's a Ruby LSP error
 */


// Register hook for status bar clicks
editor.on("lsp_status_clicked", (data) => {
  // Only handle Ruby language clicks when there's an error
  if (data.language !== "ruby" || !rubyLspError) {
    return;
  }

  editor.debug("ruby-lsp: Status clicked, showing help popup");

  // Show action popup with install options
  editor.showActionPopup({
    id: "ruby-lsp-help",
    title: "Ruby Language Server Not Found",
    message: `"${rubyLspError.serverCommand}" provides code completion, diagnostics, and navigation for Ruby files. Requires Ruby/RubyGems. Copy a command below to install it, or visit https://solargraph.org/guides/getting-started for details. Alternative: Shopify's ruby-lsp (https://github.com/Shopify/ruby-lsp).`,
    actions: [
      { id: "copy_gem", label: `Copy: ${INSTALL_COMMANDS.gem}` },
      { id: "copy_bundler", label: `Copy: ${INSTALL_COMMANDS.bundler}` },
      { id: "copy_alt", label: `Alternative: ${ALT_INSTALL}` },
      { id: "disable", label: "Disable Ruby LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});

/**
 * Handle action popup results for Ruby LSP help
 */


// Register hook for action popup results
editor.on("action_popup_result", (data) => {
  // Only handle our popup
  if (data.popup_id !== "ruby-lsp-help") {
    return;
  }

  editor.debug(`ruby-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_gem":
      editor.setClipboard(INSTALL_COMMANDS.gem);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.gem);
      break;

    case "copy_bundler":
      editor.setClipboard(INSTALL_COMMANDS.bundler);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.bundler);
      break;

    case "copy_alt":
      editor.setClipboard(ALT_INSTALL);
      editor.setStatus("Copied: " + ALT_INSTALL);
      break;

    case "disable":
      editor.disableLspForLanguage("ruby");
      editor.setStatus("Ruby LSP disabled");
      rubyLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      // Just close the popup without action
      break;

    default:
      editor.debug(`ruby-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("ruby-lsp: Plugin loaded");
