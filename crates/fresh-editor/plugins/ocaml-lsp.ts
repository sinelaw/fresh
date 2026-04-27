/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * OCaml LSP Helper Plugin
 *
 * Server: ocaml-lsp-server (binary: ocamllsp)
 * VS Code: "OCaml Platform" extension
 * Neovim: nvim-lspconfig ocamllsp
 * Install via: opam (OCaml package manager)
 * Note: Requires opam switch with merlin installed
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
  opam: "opam install ocaml-lsp-server",
  brew_opam: "brew install opam && opam init && opam install ocaml-lsp-server",
  nix: "nix-env -iA nixpkgs.ocaml-lsp",
};

let ocamlLspError: { serverCommand: string; message: string } | null = null;


editor.on("lsp_server_error", (data) => {
  if (data.language !== "ocaml") {
    return;
  }

  editor.debug(`ocaml-lsp: Server error - ${data.error_type}: ${data.message}`);

  ocamlLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `OCaml LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`OCaml LSP error: ${data.message}`);
  }
});


editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "ocaml" || !ocamlLspError) {
    return;
  }

  editor.debug("ocaml-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "ocaml-lsp-help",
    title: "OCaml Language Server Not Found",
    message: `"${ocamlLspError.serverCommand}" provides completion, diagnostics, type info, and refactoring for OCaml. Built on merlin.\n\nRequires opam (OCaml package manager). Install ocaml-lsp-server in your current opam switch.\nVS Code users: Install the "OCaml Platform" extension.\nSee: https://github.com/ocaml/ocaml-lsp`,
    actions: [
      { id: "copy_opam", label: `Copy: ${INSTALL_COMMANDS.opam}` },
      { id: "copy_nix", label: `Copy: ${INSTALL_COMMANDS.nix}` },
      { id: "disable", label: "Disable OCaml LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});


editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "ocaml-lsp-help") {
    return;
  }

  editor.debug(`ocaml-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_opam":
      editor.setClipboard(INSTALL_COMMANDS.opam);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.opam);
      break;

    case "copy_nix":
      editor.setClipboard(INSTALL_COMMANDS.nix);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.nix);
      break;

    case "disable":
      editor.disableLspForLanguage("ocaml");
      editor.setStatus("OCaml LSP disabled");
      ocamlLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`ocaml-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("ocaml-lsp: Plugin loaded");
