/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();


/**
 * Rust LSP Helper Plugin
 *
 * Provides user-friendly error handling for Rust LSP server issues.
 * When rust-analyzer fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects Rust LSP server errors (rust-analyzer)
 * - Shows popup with install commands (rustup, brew)
 * - Allows copying install commands to clipboard
 * - Provides option to disable Rust LSP
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

// Install commands for Rust LSP server
// rustup is the official recommended method
// brew is a good alternative for macOS users
// See: https://rust-analyzer.github.io/book/installation.html
const INSTALL_COMMANDS = {
  rustup: "rustup component add rust-analyzer",
  brew: "brew install rust-analyzer",
};

// Stable plugin id used as the namespace for our menu contributions
// and as the prefix on every `action_popup_result.action_id` we
// receive back from the editor.
const PLUGIN_ID = "rust-lsp";

// Track error state for Rust LSP so the menu contributions can be
// installed (when there's an error) or cleared (after recovery).
let rustLspError: { serverCommand: string; message: string } | null = null;

/**
 * Install the "fix-it" rows into the LSP-Servers popup for `rust`.
 * Mirrors the previous `showActionPopup` payload: copy-install
 * commands and a disable shortcut. Re-call with an empty array to
 * clear our slice.
 *
 * Implements the merge half of #1941 follow-up "Option B": we no
 * longer push our own separate popup; instead the editor's built-in
 * LSP-Servers popup includes our rows under a "Plugin actions"
 * section.
 */
function publishMenuContributions(): void {
  if (rustLspError === null) {
    editor.setLspMenuContributions(PLUGIN_ID, "rust", []);
    return;
  }
  editor.setLspMenuContributions(PLUGIN_ID, "rust", [
    { id: "copy_rustup", label: `Copy: ${INSTALL_COMMANDS.rustup}` },
    { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
    { id: "disable", label: "Disable Rust LSP" },
  ]);
}

/**
 * Handle LSP server errors for Rust
 */


// Register hook for LSP server errors
editor.on("lsp_server_error", (data) => {
  // Only handle Rust language errors
  if (data.language !== "rust") {
    return;
  }

  editor.debug(`rust-lsp: Server error - ${data.error_type}: ${data.message}`);

  // Store error state for later reference, install fix-it rows into
  // the LSP-Servers popup.
  rustLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };
  publishMenuContributions();

  // Show a status message for immediate feedback
  if (data.error_type === "not_found") {
    editor.setStatus(
      `Rust LSP server '${data.server_command}' not found. Click the LSP indicator for help.`
    );
  } else {
    editor.setStatus(`Rust LSP error: ${data.message}`);
  }
});

/**
 * Detect recovery and clear stale fix-it rows
 */


// Register hook for status bar clicks — used here ONLY to detect
// LSP recovery and clear stale contributions. The actual "fix-it"
// popup is the editor's built-in LSP-Servers popup with our
// contributed rows merged in (no more separate popup).
editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "rust") {
    return;
  }

  // Recovery: editor now reports no error for rust → LSP came back
  // up (e.g. successful auto-restart after an external kill). Clear
  // our error state and remove the fix-it rows from the popup so
  // the user just sees the standard server actions. (#1941 issue 3)
  if (!data.has_error && rustLspError !== null) {
    editor.debug("rust-lsp: LSP recovered; clearing rustLspError + menu rows");
    rustLspError = null;
    publishMenuContributions();
  }
});

/**
 * Handle action popup results for Rust LSP help
 */


// Register hook for action popup results
editor.on("action_popup_result", (data) => {
  editor.debug(
    `rust-lsp: action_popup_result received - popup_id=${data.popup_id}, action_id=${data.action_id}`
  );

  // The editor routes contributed-row picks with `popup_id =
  // "lsp_status"` and `action_id = "{plugin_id}|{item_id}"`.
  const prefix = `${PLUGIN_ID}|`;
  if (data.popup_id !== "lsp_status" || !data.action_id.startsWith(prefix)) {
    return;
  }
  const itemId = data.action_id.slice(prefix.length);

  editor.debug(`rust-lsp: Action selected - ${itemId}`);

  switch (itemId) {
    case "copy_rustup":
      editor.setClipboard(INSTALL_COMMANDS.rustup);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.rustup);
      break;

    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "disable":
      editor.disableLspForLanguage("rust");
      editor.setStatus("Rust LSP disabled");
      rustLspError = null;
      publishMenuContributions();
      break;

    default:
      editor.debug(`rust-lsp: Unknown action: ${itemId}`);
  }
});

// =====================================================================
// Rust LSP mode switching (Full vs Reduced Memory)
// =====================================================================

// Reduced-memory init options for rust-analyzer:
// - checkOnSave: false - disables cargo check on every save (#1 cause of slowdowns)
// - cachePriming.enable: false - no background indexing of entire crate graph
// - procMacro.enable: false - no proc-macro expansion (saves CPU/RAM)
// - cargo.buildScripts.enable: false - no build.rs
// - cargo.autoreload: false - manual reload only
const REDUCED_MEMORY_INIT_OPTIONS = {
  checkOnSave: false,
  cachePriming: { enable: false },
  procMacro: { enable: false },
  cargo: {
    buildScripts: { enable: false },
    autoreload: false,
  },
  diagnostics: { enable: true },
  files: { watcher: "server" },
};

const REDUCED_MEMORY_PROCESS_LIMITS: ProcessLimitsPackConfig = {
  maxMemoryPercent: 50,
  maxCpuPercent: 90,
  enabled: true,
};

const NO_PROCESS_LIMITS: ProcessLimitsPackConfig = {
  maxMemoryPercent: null,
  maxCpuPercent: null,
  enabled: false,
};

function on_rust_lsp_configure(): void {
  editor.showActionPopup({
    id: "rust-lsp-mode",
    title: "Rust LSP Mode",
    message: "This will override your Rust LSP config and restart the server.",
    actions: [
      { id: "full", label: "Full Mode (all features, no process limits)" },
      { id: "reduced", label: "Reduced Memory (restricted features, 50% RAM / 90% CPU limits)" },
      { id: "dismiss", label: "Cancel (ESC)" },
    ],
  });
}
registerHandler("on_rust_lsp_configure", on_rust_lsp_configure);

editor.registerCommand(
  "Rust LSP: Configure Mode",
  "Switch rust-analyzer between full and reduced memory modes",
  "on_rust_lsp_configure",
  null
);


editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "rust-lsp-mode") {
    return;
  }

  switch (data.action_id) {
    case "full":
      editor.registerLspServer("rust", {
        command: "rust-analyzer",
        args: [],
        autoStart: true,
        initializationOptions: null,
        processLimits: NO_PROCESS_LIMITS,
      });
      editor.restartLspForLanguage("rust");
      editor.setStatus("Rust LSP: Full mode — all features enabled, no process limits");
      break;

    case "reduced":
      editor.registerLspServer("rust", {
        command: "rust-analyzer",
        args: [],
        autoStart: true,
        initializationOptions: REDUCED_MEMORY_INIT_OPTIONS,
        processLimits: REDUCED_MEMORY_PROCESS_LIMITS,
      });
      editor.restartLspForLanguage("rust");
      editor.setStatus("Rust LSP: Reduced Memory mode — checkOnSave, procMacro, cachePriming disabled");
      break;

    case "dismiss":
    case "dismissed":
      break;
  }
});

editor.debug("rust-lsp: Plugin loaded");
