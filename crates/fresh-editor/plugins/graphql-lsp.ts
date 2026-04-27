/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * GraphQL LSP Helper Plugin
 *
 * Provides user-friendly error handling for GraphQL LSP server issues.
 * When graphql-lsp fails to start, this plugin shows an actionable
 * popup with installation instructions.
 *
 * Features:
 * - Detects GraphQL LSP server errors (graphql-lsp)
 * - Shows popup with install commands (npm)
 * - Provides option to disable GraphQL LSP
 *
 * VS Code: "GraphQL: Language Feature Support" extension
 * Neovim: nvim-lspconfig graphql
 * Note: Requires a graphql-config file (.graphqlrc, graphql.config.js, etc.)
 * Alternative: Apollo GraphQL extension (for Apollo users)
 * Config docs: https://the-guild.dev/graphql/config
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

// Install commands for GraphQL LSP server
// See: https://github.com/graphql/graphiql/tree/main/packages/graphql-language-service-cli
const INSTALL_COMMANDS = {
  npm: "npm i -g graphql-language-service-cli",
  yarn: "yarn global add graphql-language-service-cli",
};

// Track error state for GraphQL LSP
let graphqlLspError: { serverCommand: string; message: string } | null = null;

/**
 * Handle LSP server errors for GraphQL
 */

editor.on("lsp_server_error", (data) => {
  if (data.language !== "graphql") {
    return;
  }

  editor.debug(`graphql-lsp: Server error - ${data.error_type}: ${data.message}`);

  graphqlLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `GraphQL LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`GraphQL LSP error: ${data.message}`);
  }
});

/**
 * Handle status bar click when there's a GraphQL LSP error
 */

editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "graphql" || !graphqlLspError) {
    return;
  }

  editor.debug("graphql-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "graphql-lsp-help",
    title: "GraphQL Language Server Not Found",
    message: `"${graphqlLspError.serverCommand}" provides code completion, validation, and hover info for GraphQL schemas and queries. Requires Node.js and a .graphqlrc config. Copy a command below to install it, or visit https://github.com/graphql/graphiql/tree/main/packages/graphql-language-service-cli for details.`,
    actions: [
      { id: "copy_npm", label: `Copy: ${INSTALL_COMMANDS.npm}` },
      { id: "copy_yarn", label: `Copy: ${INSTALL_COMMANDS.yarn}` },
      { id: "disable", label: "Disable GraphQL LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});

/**
 * Handle action popup results for GraphQL LSP help
 */

editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "graphql-lsp-help") {
    return;
  }

  editor.debug(`graphql-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_npm":
      editor.setClipboard(INSTALL_COMMANDS.npm);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.npm);
      break;

    case "copy_yarn":
      editor.setClipboard(INSTALL_COMMANDS.yarn);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.yarn);
      break;

    case "disable":
      editor.disableLspForLanguage("graphql");
      editor.setStatus("GraphQL LSP disabled");
      graphqlLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`graphql-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("graphql-lsp: Plugin loaded");
