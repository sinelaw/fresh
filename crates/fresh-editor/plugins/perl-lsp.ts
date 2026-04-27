/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Perl LSP Helper Plugin
 *
 * Server: PerlNavigator (github.com/bscan/PerlNavigator)
 * VS Code: "Perl Navigator" extension
 * Neovim: nvim-lspconfig perlnavigator
 * Alternative: Perl::LanguageServer (cpan, older)
 * Note: PerlNavigator includes perlcritic and perltidy integration
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
  npm: "npm install -g perlnavigator-server",
  cpan_alt: "cpanm Perl::LanguageServer",
};

let perlLspError: { serverCommand: string; message: string } | null = null;


editor.on("lsp_server_error", (data) => {
  if (data.language !== "perl") {
    return;
  }

  editor.debug(`perl-lsp: Server error - ${data.error_type}: ${data.message}`);

  perlLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Perl LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Perl LSP error: ${data.message}`);
  }
});


editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "perl" || !perlLspError) {
    return;
  }

  editor.debug("perl-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "perl-lsp-help",
    title: "Perl Language Server Not Found",
    message: `"${perlLspError.serverCommand}" (PerlNavigator) provides completion, diagnostics, navigation, and perlcritic/perltidy integration for Perl.\n\nAlternative: Perl::LanguageServer (older, via CPAN).\nVS Code users: Install the "Perl Navigator" extension.\nSee: https://github.com/bscan/PerlNavigator`,
    actions: [
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "copy_cpan", label: `Copy: ${INSTALL_COMMANDS.cpan_alt}` },
      { id: "disable", label: "Disable Perl LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});


editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "perl-lsp-help") {
    return;
  }

  editor.debug(`perl-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_npm":
      editor.setClipboard(INSTALL_COMMANDS.npm);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.npm);
      break;

    case "copy_cpan":
      editor.setClipboard(INSTALL_COMMANDS.cpan_alt);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.cpan_alt);
      break;

    case "disable":
      editor.disableLspForLanguage("perl");
      editor.setStatus("Perl LSP disabled");
      perlLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`perl-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("perl-lsp: Plugin loaded");
