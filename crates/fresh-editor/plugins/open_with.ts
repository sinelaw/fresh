/// <reference path="./lib/fresh.d.ts" />

const editor = getEditor();

/**
 * Open With…: open a file as text, in a viewer another plugin provides, or
 * in the app this computer uses for its type.
 *
 * Offered on files in the file explorer's right-click menu, on tabs in the
 * tab menu, and as a command for the active tab's file. Each opens a picker
 * listing the ways the file can open, the one it is shown in now marked.
 *
 * A file keeps one tab (`openFileView`): picking another way to show a file
 * that is open replaces its tab in place, unless the text has unsaved
 * changes, and opening the file any other way goes to that tab.
 *
 * The menu items are registered once, for every window; each use acts in the
 * window it came from. A file is a `WindowPath` throughout: the same path in
 * two windows (one local, one over SSH) can be two different files.
 *
 * Viewers come from other plugins, through the API exported below. This
 * plugin ships with the editor and loads before the user's plugins, so a
 * viewer registers when it loads:
 *
 *     editor.getPluginApi("open-with")?.registerViewer({
 *       id: "pdf-text",
 *       label: "PDF as Text",
 *       extensions: ["pdf"],
 *       open: async (req) => {
 *         // Run where the file is: on its window's machine.
 *         const machine = await editor.openMachine({ kind: "window", window: req.path.window });
 *         const { stdout } = await machine.run("pdftotext", [req.path.value, "-"]);
 *         await machine.close();
 *         editor.setVirtualBufferContent(req.bufferId, [{ text: stdout }]);
 *       },
 *     });
 */

// =============================================================================
// Public API
// =============================================================================

/** What a viewer's `open` is called with. */
export type ViewerRequest = {
  /** The file's tab: a read-only buffer to fill with
   *  `setVirtualBufferContent`. */
  bufferId: number;
  /** The file to show, on its window's filesystem: the file APIs take it as
   *  is, and `openMachine({ kind: "window", window: path.window })` runs
   *  commands where it is. */
  path: WindowPath;
  /** The viewer's `id`. */
  viewerId: string;
};

/** A way to show some file types, from another plugin. */
export type Viewer = {
  /** Unique across plugins. */
  id: string;
  /** Shown in the picker, already translated. */
  label: string;
  /** File extensions it shows, without the dot (e.g. `["pdf"]`); matched
   *  ignoring case. */
  extensions: string[];
  /** Mode for the viewer's keys (define it with `defineMode` first). */
  mode?: string;
  open(request: ViewerRequest): void | Promise<void>;
};

export type OpenWithApi = {
  /** Offer `viewer` for its file types. Registering an `id` again replaces
   *  the viewer. Returns a function that withdraws it. */
  registerViewer(viewer: Viewer): () => void;
  /** Show the picker for `path`, a file of the active window. */
  openWith(path: WindowPath): void;
};

declare global {
  interface FreshPluginRegistry {
    "open-with": OpenWithApi;
  }
}

// =============================================================================
// State
// =============================================================================

/** The ways a file can open. */
type Choice =
  | { kind: "text" }
  | { kind: "viewer"; viewer: Viewer }
  | { kind: "systemApp" };

const PROMPT_TYPE = "open-with";

const viewers = new Map<string, Viewer>();

/** The open picker: its file, and what each suggestion's value picks. */
let picker: { path: WindowPath; choices: Map<string, Choice> } | null = null;

// =============================================================================
// Helpers
// =============================================================================

function fileName(path: WindowPath): string {
  return editor.pathBasename(path.value) || path.value;
}

/** Lowercase, without the dot; `null` for a name without one. */
function extensionOf(path: WindowPath): string | null {
  const ext = editor.pathExtname(path.value);
  return ext ? ext.slice(1).toLowerCase() : null;
}

function viewersFor(path: WindowPath): Viewer[] {
  const ext = extensionOf(path);
  if (ext === null) return [];
  return [...viewers.values()].filter((v) => v.extensions.includes(ext));
}

/** How the file's tab shows it now (the editor keeps a file to one tab):
 *  `"text"`, a viewer's id, or `null` when the file has no tab. */
function shownAs(path: WindowPath): string | null {
  for (const buffer of editor.listBuffers(path.window)) {
    if (buffer.viewing?.path === path.value) return buffer.viewing.viewer_id;
    if (!buffer.is_virtual && buffer.path === path.value) return "text";
  }
  return null;
}

/** Whether the files of window `windowId` are this computer's, so its apps
 *  can open them: a window with no remote machine (a local one, or a
 *  container's, which shares this computer's files). */
function isOnThisComputer(windowId: number): boolean {
  const window = editor.listWindows().find((w) => w.id === windowId);
  return window !== undefined && !window.remote;
}

/** The file a buffer of the active window is the tab of: its own, or the
 *  one a viewer shows in it. */
function fileOfBuffer(bufferId: number): WindowPath | null {
  const info = editor.getBufferInfo(bufferId);
  const file = info?.viewing?.path || info?.path;
  return file ? editor.windowPath(editor.activeWindow(), file) : null;
}

// =============================================================================
// Opening
// =============================================================================

// Every use starts in the window in front (a menu or the palette), and the
// editor shows a file in a tab of that window — so nothing is done once the
// reader has moved to another.

function reportFailure(path: WindowPath, label: string, error: string): void {
  editor.setStatus(
    editor.t("status.viewer_failed", { viewer: label, name: fileName(path), error }),
  );
}

async function openAs(path: WindowPath, view: FileView, label: string): Promise<number | null> {
  const result = await editor.openFileView(path, view);
  switch (result.kind) {
    case "opened":
      return result.bufferId;
    case "unsavedChanges":
      editor.setStatus(editor.t("status.unsaved", { name: fileName(path) }));
      return null;
    case "failed":
      reportFailure(path, label, result.error);
      return null;
  }
}

async function openInViewer(path: WindowPath, viewer: Viewer): Promise<void> {
  const alreadyShown = shownAs(path) === viewer.id;
  const bufferId = await openAs(
    path,
    { kind: "viewer", viewerId: viewer.id, mode: viewer.mode },
    viewer.label,
  );
  // A tab that already showed the file this way keeps what it shows.
  if (bufferId === null || alreadyShown) return;
  try {
    await viewer.open({ bufferId, path, viewerId: viewer.id });
  } catch (e) {
    reportFailure(path, viewer.label, String(e));
  }
}

async function openInSystemApp(path: WindowPath): Promise<void> {
  const name = fileName(path);
  const result = await editor.openInSystemApp(path);
  switch (result.kind) {
    case "opened":
      editor.setStatus(editor.t("status.opened_in_system", { name }));
      break;
    case "notLocal":
      editor.setStatus(editor.t("status.not_local", { name }));
      break;
    case "failed":
      editor.setStatus(
        editor.t("status.system_failed", { name, error: result.error }),
      );
      break;
  }
}

async function open(path: WindowPath, choice: Choice): Promise<void> {
  if (path.window !== editor.activeWindow()) return;
  switch (choice.kind) {
    case "text":
      await openAs(path, { kind: "text" }, editor.t("choice.text"));
      return;
    case "viewer":
      return openInViewer(path, choice.viewer);
    case "systemApp":
      return openInSystemApp(path);
  }
}

// =============================================================================
// Picker
// =============================================================================

function openWith(path: WindowPath): void {
  if (path.window !== editor.activeWindow()) return;
  const shown = shownAs(path);
  const entries: Array<{ value: string; choice: Choice; label: string; current: boolean }> = [
    {
      value: "text",
      choice: { kind: "text" },
      label: editor.t("choice.text"),
      current: shown === "text",
    },
    ...viewersFor(path).map((viewer) => ({
      value: `viewer:${viewer.id}`,
      choice: { kind: "viewer", viewer } as Choice,
      label: viewer.label,
      current: shown === viewer.id,
    })),
  ];
  if (isOnThisComputer(path.window)) {
    entries.push({
      value: "system",
      choice: { kind: "systemApp" },
      label: editor.t("choice.system_app"),
      current: false,
    });
  }

  const choices = new Map<string, Choice>();
  const suggestions: PromptSuggestion[] = entries.map((entry) => {
    choices.set(entry.value, entry.choice);
    return {
      id: entry.value,
      text: entry.label,
      description: entry.current ? editor.t("choice.current") : undefined,
      value: entry.value,
    };
  });
  picker = { path, choices };
  editor.startPrompt(
    editor.t("prompt.open_with", { name: fileName(path) }),
    PROMPT_TYPE,
  );
  editor.setPromptSuggestions(suggestions, 0);
}

editor.on("prompt_confirmed", (args) => {
  if (args.prompt_type !== PROMPT_TYPE) return;
  const pending = picker;
  picker = null;
  const choice = pending?.choices.get(args.input);
  if (!pending || !choice) return;
  open(pending.path, choice).catch((e) => editor.error(`open_with: ${e}`));
});

editor.on("prompt_cancelled", (args) => {
  if (args.prompt_type === PROMPT_TYPE) picker = null;
});

// =============================================================================
// Entry points
// =============================================================================

registerHandler("open_with_explorer", (target: FileExplorerMenuTarget) => {
  openWith(target.path);
});

registerHandler("open_with_tab", (target: TabMenuTarget) => {
  // A "fileTab" item: a viewer's tab is its file's too.
  if (target.path) openWith(target.path);
});

registerHandler("open_with_active", () => {
  const path = fileOfBuffer(editor.getActiveBufferId());
  if (path === null) {
    editor.setStatus(editor.t("status.no_file"));
    return;
  }
  openWith(path);
});

editor.addFileExplorerMenuItem({
  label: editor.t("menu.open_with"),
  scope: "file",
  handler: "open_with_explorer",
});

editor.addTabMenuItem({
  label: editor.t("menu.open_with"),
  scope: "fileTab",
  handler: "open_with_tab",
});

editor.registerCommand("%cmd.open_with", "%cmd.open_with_desc", "open_with_active");

editor.exportPluginApi("open-with", {
  registerViewer(viewer: Viewer): () => void {
    if (!viewer.id || !viewer.label || viewer.extensions.length === 0) {
      throw new Error("open-with.registerViewer: id, label and extensions are required");
    }
    if (typeof viewer.open !== "function") {
      throw new Error("open-with.registerViewer: open must be a function");
    }
    const registered: Viewer = {
      ...viewer,
      extensions: viewer.extensions.map((e) => e.replace(/^\./, "").toLowerCase()),
    };
    viewers.set(viewer.id, registered);
    return () => {
      if (viewers.get(viewer.id) === registered) viewers.delete(viewer.id);
    };
  },
  openWith,
} satisfies OpenWithApi);
