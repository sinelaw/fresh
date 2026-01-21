/// <reference path="./lib/fresh.d.ts" />
// Provides installation help when marksman (Markdown LSP) is not found
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

const INSTALL_URL = "https://github.com/artempyanykh/marksman#how-to-install";
let markdownLspError: { serverCommand: string; message: string } | null = null;

globalThis.on_markdown_lsp_server_error = function (data: LspServerErrorData): void {
  if (data.language !== "markdown") return;
  markdownLspError = { serverCommand: data.server_command, message: data.message };
  if (data.error_type === "not_found") {
    editor.setStatus(`Markdown LSP '${data.server_command}' not found. Click status bar for help.`);
  } else {
    editor.setStatus(`Markdown LSP error: ${data.message}`);
  }
};
editor.on("lsp_server_error", "on_markdown_lsp_server_error");

globalThis.on_markdown_lsp_status_clicked = function (data: LspStatusClickedData): void {
  if (data.language !== "markdown" || !markdownLspError) return;
  editor.showActionPopup({
    id: "marksman-lsp-help",
    title: "Markdown Language Server Not Found",
    message: `Install marksman for wiki-links and navigation. Visit ${INSTALL_URL}`,
    actions: [
      { id: "copy_url", label: "Copy install URL" },
      { id: "disable", label: "Disable Markdown LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
};
editor.on("lsp_status_clicked", "on_markdown_lsp_status_clicked");

globalThis.on_markdown_lsp_action_result = function (data: ActionPopupResultData): void {
  if (data.popup_id !== "marksman-lsp-help") return;
  switch (data.action_id) {
    case "copy_url":
      editor.setClipboard(INSTALL_URL);
      editor.setStatus("Copied: " + INSTALL_URL);
      break;
    case "disable":
      editor.disableLspForLanguage("markdown");
      editor.setStatus("Markdown LSP disabled");
      markdownLspError = null;
      break;
  }
};
editor.on("action_popup_result", "on_markdown_lsp_action_result");
