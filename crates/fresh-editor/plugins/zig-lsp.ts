/// <reference path="./lib/fresh.d.ts" />
// Provides installation help when zls (Zig LSP) is not found
const editor = getEditor();

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

const INSTALL_URL = "https://github.com/zigtools/zls#installation";
let zigLspError: { serverCommand: string; message: string } | null = null;

globalThis.on_zig_lsp_server_error = function (data: LspServerErrorData): void {
  if (data.language !== "zig") return;
  zigLspError = { serverCommand: data.server_command, message: data.message };
  if (data.error_type === "not_found") {
    editor.setStatus(`Zig LSP '${data.server_command}' not found. Click status bar for help.`);
  } else {
    editor.setStatus(`Zig LSP error: ${data.message}`);
  }
};
editor.on("lsp_server_error", "on_zig_lsp_server_error");

globalThis.on_zig_lsp_status_clicked = function (data: LspStatusClickedData): void {
  if (data.language !== "zig" || !zigLspError) return;
  editor.showActionPopup({
    id: "zig-lsp-help",
    title: "Zig Language Server Not Found",
    message: `Install zls for code completion and diagnostics. Visit ${INSTALL_URL}`,
    actions: [
      { id: "copy_url", label: "Copy install URL" },
      { id: "disable", label: "Disable Zig LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
};
editor.on("lsp_status_clicked", "on_zig_lsp_status_clicked");

globalThis.on_zig_lsp_action_result = function (data: ActionPopupResultData): void {
  if (data.popup_id !== "zig-lsp-help") return;
  switch (data.action_id) {
    case "copy_url":
      editor.setClipboard(INSTALL_URL);
      editor.setStatus("Copied: " + INSTALL_URL);
      break;
    case "disable":
      editor.disableLspForLanguage("zig");
      editor.setStatus("Zig LSP disabled");
      zigLspError = null;
      break;
  }
};
editor.on("action_popup_result", "on_zig_lsp_action_result");
