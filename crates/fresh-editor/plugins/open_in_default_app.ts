/// <reference path="./lib/fresh.d.ts" />

const editor = getEditor();

/**
 * Open in Default App: open a file in the app this computer uses for its
 * type, as a file manager's double click would — an image viewer for a
 * picture, a PDF reader for a PDF.
 *
 * Offered on files in the file explorer's right-click menu, on file tabs in
 * the tab menu, and as a command for the active tab's file. Each use acts on
 * the file of the window it came from. In a window whose files are on another
 * machine (SSH, Kubernetes) this computer's apps cannot reach them: the menu
 * items are greyed out and the command is hidden there, through a context
 * this plugin keeps for the active window.
 */

/** Set while the active window's files are on this computer. */
const LOCAL_FILES = "open-in-default-app-local";

/** A window has a remote machine when its files are on an SSH or
 *  Kubernetes host; a local window's, or a container's (which shares this
 *  computer's files), are here. Plugins load before the editor lists its
 *  windows, so a window not listed yet counts as here: offering the item
 *  costs nothing, as `openInSystemApp` refuses a file that is not. */
function updateLocalFilesContext(): void {
  const id = editor.activeWindow();
  const window = editor.listWindows().find((w) => w.id === id);
  editor.setContext(LOCAL_FILES, window === undefined || !window.remote);
}

function fileName(path: WindowPath): string {
  return editor.pathBasename(path.value) || path.value;
}

async function openInDefaultApp(path: WindowPath): Promise<void> {
  const name = fileName(path);
  // The launcher can take a moment to answer.
  editor.setStatus(editor.t("status.opening", { name }));
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
  context: LOCAL_FILES,
});

editor.addTabMenuItem({
  label: editor.t("menu.open_in_default_app"),
  scope: "fileTab",
  handler: "open_in_default_app_tab",
  context: LOCAL_FILES,
});

editor.registerCommand(
  "%cmd.open_in_default_app",
  "%cmd.open_in_default_app_desc",
  "open_in_default_app_active",
  LOCAL_FILES,
);

// Contexts are kept per window, and a window's machine can change under it
// (attaching to a host): set it for whichever window is in front, and again
// once the editor is ready and lists its windows.
editor.on("ready", updateLocalFilesContext);
editor.on("active_window_changed", updateLocalFilesContext);
editor.on("authority_changed", updateLocalFilesContext);
updateLocalFilesContext();
