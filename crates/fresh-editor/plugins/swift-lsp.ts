/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Swift LSP Helper Plugin
 *
 * Server: sourcekit-lsp (bundled with Xcode/Swift toolchain)
 * VS Code: "Swift" extension by Swift Server Work Group
 * Neovim: nvim-lspconfig sourcekit
 * macOS: Included with Xcode (xcrun sourcekit-lsp)
 * Linux: Install Swift toolchain from swift.org
 * Note: For Xcode projects, also install xcode-build-server
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
  macos: "xcode-select --install",
  linux: "# Download Swift from https://swift.org/download/",
  xcode_build_server: "brew install xcode-build-server",
};

let swiftLspError: { serverCommand: string; message: string } | null = null;


editor.on("lsp_server_error", (data) => {
  if (data.language !== "swift") {
    return;
  }

  editor.debug(`swift-lsp: Server error - ${data.error_type}: ${data.message}`);

  swiftLspError = {
    serverCommand: data.server_command,
    message: data.message,
  };

  if (data.error_type === "not_found") {
    editor.setStatus(
      `Swift LSP server '${data.server_command}' not found. Click status bar for help.`
    );
  } else {
    editor.setStatus(`Swift LSP error: ${data.message}`);
  }
});


editor.on("lsp_status_clicked", (data) => {
  if (data.language !== "swift" || !swiftLspError) {
    return;
  }

  editor.debug("swift-lsp: Status clicked, showing help popup");

  editor.showActionPopup({
    id: "swift-lsp-help",
    title: "Swift Language Server Not Found",
    message: `"${swiftLspError.serverCommand}" provides completion, diagnostics, and navigation for Swift files. It is bundled with the Swift toolchain.\n\nmacOS: Install Xcode Command Line Tools. Use 'xcrun sourcekit-lsp' if not in PATH.\nLinux: Download the Swift toolchain from swift.org.\nFor Xcode projects: Install xcode-build-server for build system integration.\nVS Code users: Install the "Swift" extension.\nSee: https://github.com/swiftlang/sourcekit-lsp`,
    actions: [
      { id: "copy_macos", label: `Copy: ${INSTALL_COMMANDS.macos}` },
      { id: "copy_xbs", label: `Copy: ${INSTALL_COMMANDS.xcode_build_server}` },
      { id: "disable", label: "Disable Swift LSP" },
      { id: "dismiss", label: "Dismiss (ESC)" },
    ],
  });
});


editor.on("action_popup_result", (data) => {
  if (data.popup_id !== "swift-lsp-help") {
    return;
  }

  editor.debug(`swift-lsp: Action selected - ${data.action_id}`);

  switch (data.action_id) {
    case "copy_macos":
      editor.setClipboard(INSTALL_COMMANDS.macos);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.macos);
      break;

    case "copy_xbs":
      editor.setClipboard(INSTALL_COMMANDS.xcode_build_server);
      editor.setStatus("Copied: " + INSTALL_COMMANDS.xcode_build_server);
      break;

    case "disable":
      editor.disableLspForLanguage("swift");
      editor.setStatus("Swift LSP disabled");
      swiftLspError = null;
      break;

    case "dismiss":
    case "dismissed":
      break;

    default:
      editor.debug(`swift-lsp: Unknown action: ${data.action_id}`);
  }
});

editor.debug("swift-lsp: Plugin loaded");
