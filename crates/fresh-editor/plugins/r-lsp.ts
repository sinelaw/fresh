/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * R LSP Helper Plugin
 *
 * Server: languageserver (R package)
 * VS Code: "R" extension by REditorSupport (uses languageserver)
 * Neovim: nvim-lspconfig r_language_server
 * Install via: R's install.packages() - runs as an R script
 * Note: Also install httpgd for plot viewer support
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

const INSTALL_COMMANDS = {
  r: 'R -e \'install.packages("languageserver")\'',
  conda: "conda install -c conda-forge r-languageserver",
};

let rLspError: { serverCommand: string; message: string } | null = null;


editor.on("lsp_server_error", (data) => {
  if (data.language !== "r") {
    return;
  }

  editor.debug(`r-lsp: Server error - ${data.error_type}: ${data.message}`);

  rLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `R LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`R LSP error: ${data.message}`);
  }
});


editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "r" || !rLspError) {
    return;
  }

  editor.debug("r-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "r-lsp-help",
    title: "R Language Server Not Found",
    message: `The R language server provides completion, diagnostics, hover, formatting, and go-to-definition for R files. It runs as an R script, so R must be installed.\n\nInstall the languageserver R package, then the server runs via: R --vanilla -e 'languageserver::run()'\nVS Code users: Install the "R" extension by REditorSupport.\nSee: https://github.com/REditorSupport/languageserver`,
    actions: [
      { id: "copy_r", label: `Copy: ${INSTALL_COMMANDS.r}` },
      { id: "copy_conda", label: `Copy: ${INSTALL_COMMANDS.conda}` },
      { id: "disable", label: "Disable R LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});


editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "r-lsp-help") {
    return;
  }

  editor.debug(`r-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_r":
      editor.setClipboard(INSTALL_COMMANDS.r);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.r);
      break;

    case "copy_conda":
      editor.setClipboard(INSTALL_COMMANDS.conda);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.conda);
      break;

    case "disable":
      editor.disableLspForLanguage("r");
      editor.setStatus("R LSP disabled");
      rLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`r-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("r-lsp: Plugin loaded");
