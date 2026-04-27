/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * SQL LSP Helper Plugin
 *
 * Provides user-friendly error handling for SQL LSP server issues.
 * When sqls fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Server: sqls (github.com/sqls-server/sqls)
 * VS Code: SQLTools extension, Database Client JDBC
 * Neovim: nvim-lspconfig sqls
 * Note: sqls needs a config.yml for database connections
 * Alternative: sql-language-server (npm)
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
  go: "go install github.com/sqls-server/sqls@latest",
  brew: "brew install sqls",
  npm_alt: "npm install -g sql-language-server",
};

let sqlLspError: { serverCommand: string; message: string } | null = null;


editor.on("lsp_server_error", (data) => {
  if (data.language !== "sql") {
    return;
  }

  editor.debug(`sql-lsp: Server error - ${data.error_type}: ${data.message}`);

  sqlLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `SQL LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`SQL LSP error: ${data.message}`);
  }
});


editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "sql" || !sqlLspError) {
    return;
  }

  editor.debug("sql-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "sql-lsp-help",
    title: "SQL Language Server Not Found",
    message: `"${sqlLspError.serverCommand}" provides completion, hover, and diagnostics for SQL files. It requires a config.yml to connect to your database. See: https://github.com/sqls-server/sqls\n\nAlternative: sql-language-server (npm) supports MySQL, PostgreSQL, SQLite.\nVS Code users: Try the SQLTools extension.`,
    actions: [
      { id: "copy_go", label: `Copy: ${INSTALL_COMMANDS.go}` },
      { id: "copy_brew", label: `Copy: ${INSTALL_COMMANDS.brew}` },
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm_alt}` },
      { id: "disable", label: "Disable SQL LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});


editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "sql-lsp-help") {
    return;
  }

  editor.debug(`sql-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_go":
      editor.setClipboard(INSTALL_COMMANDS.go);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.go);
      break;

    case "copy_brew":
      editor.setClipboard(INSTALL_COMMANDS.brew);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.brew);
      break;

    case "copy_npm":
      editor.setClipboard(INSTALL_COMMANDS.npm_alt);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.npm_alt);
      break;

    case "disable":
      editor.disableLspForLanguage("sql");
      editor.setStatus("SQL LSP disabled");
      sqlLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`sql-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("sql-lsp: Plugin loaded");
