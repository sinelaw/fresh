/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Erlang LSP Helper Plugin
 *
 * Server: erlang_ls (github.com/erlang-ls/erlang_ls)
 * VS Code: "Erlang LS" extension
 * Neovim: nvim-lspconfig erlangls
 * Requires: Erlang/OTP 24+, rebar3
 * Note: erlang_ls is archived; consider ELP (Erlang Language Platform)
 * by WhatsApp as the recommended successor
 * ELP: github.com/WhatsApp/erlang-language-platform
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
  brew: "brew install erlang_ls",
  build: "git clone https://github.com/erlang-ls/erlang_ls && cd erlang_ls && make && make install",
  nix: "nix-env -iA nixpkgs.erlang-ls",
};

let erlangLspError: { serverCommand: string; message: string } | null = null;


editor.on("lsp_server_error", (data) => {
  if (data.language !== "erlang") {
    return;
  }

  editor.debug(`erlang-lsp: Server error - ${data.error_type}: ${data.message}`);

  erlangLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Erlang LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Erlang LSP error: ${data.message}`);
  }
});


editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "erlang" || !erlangLspError) {
    return;
  }

  editor.debug("erlang-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "erlang-lsp-help",
    title: "Erlang Language Server Not Found",
    message: `"${erlangLspError.serverCommand}" provides completion, diagnostics, navigation, and code actions for Erlang. Requires Erlang/OTP 24+.\n\nNote: erlang_ls is archived. Consider ELP (Erlang Language Platform) by WhatsApp as the successor: https://github.com/WhatsApp/erlang-language-platform\nConfigure via erlang_ls.config in your project root.\nVS Code users: Install "Erlang LS" or "Erlang Language Platform" extension.\nSee: https://github.com/erlang-ls/erlang_ls`,
    actions: [
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_nix", label: `Copy: ${INSTALL_COMMANDS.nix}` },
      { id: "disable", label: "Disable Erlang LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});


editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "erlang-lsp-help") {
    return;
  }

  editor.debug(`erlang-lsp: Action selected - ${data.action_id}`);

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
      editor.disableLspForLanguage("erlang");
      editor.setStatus("Erlang LSP disabled");
      erlangLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`erlang-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("erlang-lsp: Plugin loaded");
