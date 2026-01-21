/// <reference path="./lib/fresh.d.ts" />
// Provides installation help when templ LSP is not found
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

const INSTALL_URL = "https://templ.guide/quick-start/installation";
let templLspError: { serverCommand: string; message: string } | null = null;

globalThis.on_templ_lsp_server_error = function (data: LspServerErrorData): void {
  if (data.language !== "templ") return;
  templLspError = { serverCommand: data.server_command, message: data.message };
  if (data.error_type === "not_found") {
    editor.setStatus(`Templ LSP '${data.server_command}' not found. Click status bar for help.`);
  } else {
    editor.setStatus(`Templ LSP error: ${data.message}`);
  }
};
editor.on("lsp_server_error", "on_templ_lsp_server_error");

globalThis.on_templ_lsp_status_clicked = function (data: LspStatusClickedData): void {
  if (data.language !== "templ" || !templLspError) return;
  editor.showActionPopup({
    id: "templ-lsp-help",
    title: "Templ Language Server Not Found",
    message: `Install templ for code completion and diagnostics. Visit ${INSTALL_URL}`,
    actions: [
      { id: "copy_url", label: "Copy install URL" },
      { id: "disable", label: "Disable Templ LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
};
editor.on("lsp_status_clicked", "on_templ_lsp_status_clicked");

globalThis.on_templ_lsp_action_result = function (data: ActionPopupResultData): void {
  if (data.popup_id !== "templ-lsp-help") return;
  switch (data.action_id) {
    case "copy_url":
      editor.setClipboard(INSTALL_URL);
      editor.setStatus("Copied: " + INSTALL_URL);
      break;
    case "disable":
      editor.disableLspForLanguage("templ");
      editor.setStatus("Templ LSP disabled");
      templLspError = null;
      break;
  }
};
editor.on("action_popup_result", "on_templ_lsp_action_result");
