/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Terraform LSP Helper Plugin
 *
 * Provides user-friendly error handling for Terraform LSP server issues.
 * When terraform-ls fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects Terraform LSP server errors (terraform-ls)
 * - Shows popup with install commands (brew, choco, etc.)
 * - Provides option to disable Terraform LSP
 *
 * VS Code: "HashiCorp Terraform" extension (uses terraform-ls)
 * Neovim: nvim-lspconfig terraformls
 * Also supports: Terraform, Terraform variables, and tfvars files
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

// Install commands for Terraform LSP server (terraform-ls by HashiCorp)
// See: https://github.com/hashicorp/terraform-ls
const INSTALL_COMMANDS = {
  brew: "brew install hashicorp/tap/terraform-ls",
  choco: "choco install terraform-ls",
  nix: "nix-env -i terraform-ls",
};

// Track error state for Terraform LSP
let terraformLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for Terraform
 */

editor.on("lsp_server_error", (data) => {
  if (data.language !== "terraform") {
    return;
  }

  editor.debug(`terraform-lsp: Server error - ${data.error_type}: ${data.message}`);

  terraformLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Terraform LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Terraform LSP error: ${data.message}`);
  }
});

/**
 * Handle status bar click when there's a Terraform LSP error
 */

editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "terraform" || !terraformLspError) {
    return;
  }

  editor.debug("terraform-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "terraform-lsp-help",
    title: "Terraform Language Server Not Found",
    message: `"${terraformLspError.serverCommand}" (by HashiCorp) provides code completion, diagnostics, and navigation for Terraform files. Copy a command below to install it, or visit https://github.com/hashicorp/terraform-ls for details and pre-built binaries.`,
    actions: [
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_choco", label: `Copy: ${INSTALL_COMMANDS.choco} (Windows)` },
      { id: "copy_nix", label: `Copy: ${INSTALL_COMMANDS.nix}` },
      { id: "disable", label: "Disable Terraform LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});

/**
 * Handle action popup results for Terraform LSP help
 */

editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "terraform-lsp-help") {
    return;
  }

  editor.debug(`terraform-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_choco":
      editor.setClipboard(INSTALL_COMMANDS.choco);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.choco);
      break;

    case "copy_nix":
      editor.setClipboard(INSTALL_COMMANDS.nix);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.nix);
      break;

    case "disable":
      editor.disableLspForLanguage("terraform");
      editor.setStatus("Terraform LSP disabled");
      terraformLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`terraform-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("terraform-lsp: Plugin loaded");
