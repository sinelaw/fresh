/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Elixir LSP Helper Plugin
 *
 * Server: elixir-ls (github.com/elixir-lsp/elixir-ls)
 * VS Code: "ElixirLS" extension
 * Neovim: nvim-lspconfig elixirls
 * Requires: Elixir and Erlang/OTP installed
 * Note: The Elixir ecosystem is consolidating around Expert
 * (github.com/elixir-lang/expert) as the new official LSP server
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
  github: "# Download from https://github.com/elixir-lsp/elixir-ls/releases",
  brew: "brew install elixir-ls",
  nix: "nix-env -iA nixpkgs.elixir-ls",
};

let elixirLspError: { serverCommand: string; message: string } | null = null;


editor.on("lsp_server_error", (data) => {
  if (data.language !== "elixir") {
    return;
  }

  editor.debug(`elixir-lsp: Server error - ${data.error_type}: ${data.message}`);

  elixirLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Elixir LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Elixir LSP error: ${data.message}`);
  }
});


editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "elixir" || !elixirLspError) {
    return;
  }

  editor.debug("elixir-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "elixir-lsp-help",
    title: "Elixir Language Server Not Found",
    message: `"${elixirLspError.serverCommand}" provides completion, diagnostics, go-to-definition, Dialyzer integration, and debugging for Elixir. Requires Elixir and Erlang/OTP.\n\nNew: Expert (https://expert-lsp.org) is the upcoming official Elixir LSP, merging ElixirLS, Lexical, and Next LS.\nVS Code users: Install the "ElixirLS" extension.\nSee: https://github.com/elixir-lsp/elixir-ls`,
    actions: [
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_nix", label: `Copy: ${INSTALL_COMMANDS.nix}` },
      { id: "disable", label: "Disable Elixir LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});


editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "elixir-lsp-help") {
    return;
  }

  editor.debug(`elixir-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_nix":
      editor.setClipboard(INSTALL_COMMANDS.nix);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.nix);
      break;

    case "disable":
      editor.disableLspForLanguage("elixir");
      editor.setStatus("Elixir LSP disabled");
      elixirLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`elixir-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("elixir-lsp: Plugin loaded");
