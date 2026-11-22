/// <reference path="./lib/fresh.d.ts" />

const editor = getEditor();

/**
 * Open in Default App: open a file in the app this computer uses for its
 * type, as a file manager's double click would — an image viewer for a
 * picture, a PDF reader for a PDF.
 *
 * Offered on files in the file explorer's right-click menu, on file tabs in
 * the tab menu, and as a command for the active tab's file. Each use acts on
 * the file of the window it came from; a window whose files are on another
 * machine (SSH, Kubernetes) gets a message instead, as this computer's apps
 * cannot reach them.
 */

function fileName(path: WindowPath): string {
  return editor.pathBasename(path.value) || path.value;
}

async function openInDefaultApp(path: WindowPath): Promise<void> {
  const name = fileName(path);
  const result = await editor.openInSystemApp(path);
  switch (result.kind) {
    case "opened":
      editor.setStatus(editor.t("status.opened", { name }));
      break;
    case "notLocal":
      editor.setStatus(editor.t("status.not_local", { name }));
      break;
    case "failed":
      editor.setStatus(editor.t("status.failed", { name, error: result.error }));
      break;
  }
}

function report(e: unknown): void {
  editor.error(`open_in_default_app: ${e}`);
}

registerHandler("open_in_default_app_explorer", (target: FileExplorerMenuTarget) => {
  openInDefaultApp(target.path).catch(report);
});

registerHandler("open_in_default_app_tab", (target: TabMenuTarget) => {
  // A "fileTab" item: the target always has a file.
  if (target.path) openInDefaultApp(target.path).catch(report);
});

registerHandler("open_in_default_app_active", () => {
  const file = editor.getBufferPath(editor.getActiveBufferId());
  if (!file) {
    editor.setStatus(editor.t("status.no_file"));
    return;
  }
  openInDefaultApp(editor.windowPath(editor.activeWindow(), file)).catch(report);
});

editor.addFileExplorerMenuItem({
  label: editor.t("menu.open_in_default_app"),
  scope: "file",
  handler: "open_in_default_app_explorer",
});

editor.addTabMenuItem({
  label: editor.t("menu.open_in_default_app"),
  scope: "fileTab",
  handler: "open_in_default_app_tab",
});

editor.registerCommand(
  "%cmd.open_in_default_app",
  "%cmd.open_in_default_app_desc",
  "open_in_default_app_active",
);
