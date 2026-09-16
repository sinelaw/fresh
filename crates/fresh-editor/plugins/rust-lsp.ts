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


// ─── rust-analyzer client commands (CodeLens runnables) ──────────────────
//
// rust-analyzer's "▶︎ Run"/"▶︎ Run Test" CodeLens entries carry commands the
// *client* executes, not the server: `rust-analyzer.runSingle` is absent
// from the server's `executeCommandProvider` list, so sending it back with
// `workspace/executeCommand` would just be rejected. LSP does not define
// what these mean — the payload is rust-analyzer's own `Runnable` type — so
// the editor core deliberately interprets none of it. We claim the names
// here and translate them ourselves.
//
// Claiming also *enables* the lenses: rust-analyzer omits runnable CodeLens
// entirely unless the client advertises these commands at `initialize`
// (which the core does from the registry this feeds).
//
// Ref: rust-analyzer's documented LSP extensions, "Client Commands".

const RUN_SINGLE = "rust-analyzer.runSingle";

/** rust-analyzer's `Runnable`: a cargo invocation or a bare program. */
type Runnable = {
  label: string;
} & (
  | {
      kind: "cargo";
      args: {
        environment?: Record<string, string>;
        cwd: string;
        overrideCargo?: string | null;
        workspaceRoot?: string | null;
        cargoArgs: string[];
        executableArgs: string[];
      };
    }
  | {
      kind: "shell";
      args: {
        environment?: Record<string, string>;
        cwd: string;
        program: string;
        args: string[];
      };
    }
);

/** What we hand to `createTerminal`. */
interface RunnableTask {
  label: string;
  cwd: string;
  command: string[];
  env: Record<string, string>;
}

/**
 * Flatten a `Runnable` into an argv.
 *
 * The edge cases worth knowing: `overrideCargo` is a command line rather
 * than a path so it may be several words, `--` separates only when there
 * are executable args, and the cwd is the workspace root when one is given.
 * Covered end-to-end by `e2e::plugins::lsp_client_commands`, which points
 * `overrideCargo` at a recorder script and asserts the exact argv.
 */
function runnableToTask(runnable: Runnable): RunnableTask {
  if (runnable.kind === "cargo") {
    const a = runnable.args;
    // `overrideCargo` is a command line, not a path: "cargo +nightly".
    const cargo = (a.overrideCargo ?? "cargo").split(/\s+/).filter((w) => w.length > 0);
    if (cargo.length === 0) {
      cargo.push("cargo");
    }
    const command = [...cargo, ...a.cargoArgs];
    if (a.executableArgs.length > 0) {
      command.push("--", ...a.executableArgs);
    }
    return {
      label: runnable.label,
      // Run from the workspace root when rust-analyzer names one, so the
      // invocation matches what it would run from the crate's workspace.
      cwd: a.workspaceRoot ?? a.cwd,
      command,
      env: a.environment ?? {},
    };
  }

  const a = runnable.args;
  return {
    label: runnable.label,
    cwd: a.cwd,
    command: [a.program, ...a.args],
    env: a.environment ?? {},
  };
}

// One call for the whole set: a claim that arrives after a server started
// costs a restart (LSP capabilities are fixed at `initialize`), and claiming
// them together keeps that to at most one.
//
// `rust-analyzer.debugSingle` is deliberately NOT claimed. Claiming is what
// makes rust-analyzer emit a lens at all, so claiming it would put a
// "⚙︎ Debug" lens on every runnable that could only ever answer "fresh has
// no debugger" (#988). Add it here when there is something to hand it to.
editor.registerLspClientCommands([RUN_SINGLE]);

editor.on("lsp_execute_command", async (data) => {
  if (data.command !== RUN_SINGLE) {
    return; // claimed by some other plugin
  }

  let runnable: Runnable;
  try {
    const args: unknown = JSON.parse(data.arguments ?? "[]");
    if (!Array.isArray(args) || args.length === 0) {
      throw new Error("no arguments");
    }
    // Read the discriminant off an untyped view: narrowing `Runnable`
    // itself would leave `never` in the failure branch, where the actual
    // value is exactly what we want to report.
    const kind: unknown = (args[0] as { kind?: unknown })?.kind;
    if (kind !== "cargo" && kind !== "shell") {
      throw new Error(`unsupported runnable kind '${String(kind)}'`);
    }
    runnable = args[0] as Runnable;
  } catch (e) {
    editor.setStatus(`Rust LSP: could not read runnable for '${data.title}': ${String(e)}`);
    return;
  }

  let task: RunnableTask;
  try {
    // Flattening reads `cargoArgs`/`args` off the payload; a runnable with
    // the right `kind` but missing those throws here, not above.
    task = runnableToTask(runnable);
  } catch (e) {
    editor.setStatus(`Rust LSP: could not read runnable for '${data.title}': ${String(e)}`);
    return;
  }

  try {
    await editor.createTerminal({
      cwd: task.cwd,
      command: task.command,
      title: task.label,
      env: task.env,
      focus: true,
      persistent: false,
    });
  } catch (e) {
    editor.setStatus(`Rust LSP: could not run '${task.label}': ${String(e)}`);
  }
});

editor.debug("rust-lsp: Plugin loaded");
