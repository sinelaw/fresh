/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Assembly LSP Helper Plugin
 *
 * Server: asm-lsp (github.com/bergercookie/asm-lsp)
 * Covers GAS/NASM/MASM across x86, x86_64, ARM and RISC-V.
 * Fresh routes both the "asm" (Intel/NASM) and "gas" (AT&T/GAS)
 * languages to it.
 * Install via: cargo (Rust package manager)
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

const ASM_LANGUAGES = ["asm", "gas"];

const INSTALL_COMMANDS = {
  cargo: "cargo install asm-lsp",
  binstall: "cargo binstall asm-lsp",
};

let asmLspError: {
  language: string;
  serverCommand: string;
  message: string;
} | null = null;


editor.on("lsp_server_error", (data) => {
  if (!ASM_LANGUAGES.includes(data.language)) {
    return;
  }

  editor.debug(`asm-lsp: Server error - ${data.error_type}: ${data.message}`);

  asmLspError = {
    language: data.language,
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Assembly LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Assembly LSP error: ${data.message}`);
  }
});


editor.on("lsp_status_clicked", (data) => {
  if (!ASM_LANGUAGES.includes(data.language) || !asmLspError) {
    return;
  }

  editor.debug("asm-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "asm-lsp-help",
    title: "Assembly Language Server Not Found",
    message: `"${asmLspError.serverCommand}" provides completion, diagnostics, hover docs for opcodes/registers/directives, and go-to-definition for assembly (GAS, NASM, MASM). Requires Rust's cargo to install.\n\nOptional per-project config: .asm-lsp.toml (choose assembler and instruction set).\nSee: https://github.com/bergercookie/asm-lsp`,
    actions: [
      { id: "copy_cargo", label: `Copy: ${INSTALL_COMMANDS.cargo}` },
      { id: "copy_binstall", label: `Copy: ${INSTALL_COMMANDS.binstall}` },
      { id: "disable", label: "Disable Assembly LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});


editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "asm-lsp-help") {
    return;
  }

  editor.debug(`asm-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_cargo":
      editor.setClipboard(INSTALL_COMMANDS.cargo);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.cargo);
      break;

    case "copy_binstall":
      editor.setClipboard(INSTALL_COMMANDS.binstall);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.binstall);
      break;

    case "disable":
      for (const language of ASM_LANGUAGES) {
        editor.disableLspForLanguage(language);
      }
      editor.setStatus("Assembly LSP disabled");
      asmLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`asm-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("asm-lsp: Plugin loaded");
