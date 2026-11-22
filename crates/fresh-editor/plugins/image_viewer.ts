/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Image Viewer
 *
 * Opening a picture (PNG, JPEG, GIF, WebP, BMP) shows the picture instead of
 * its bytes. The editor draws it (`setBufferImage`) with the terminal's own
 * graphics where it has them (Kitty, Sixel, iTerm2) and coloured half blocks
 * everywhere else, fitted to the pane.
 *
 * Keys in the viewer:
 *   + / =   zoom in          -   zoom out         0   fit the pane
 *   h j k l or arrows: move around a zoomed picture
 *   r   load the file again  t   show the file as text   q   close
 *
 * It redraws when the file changes on disk (a plot a script rewrites shows
 * the new one). Browsing pictures in the file explorer reuses one viewer
 * tab, as the explorer's preview tabs do; zooming in it, or opening the file
 * for real, keeps it.
 *
 * A build without image support (`getImageSupport()` is null) leaves
 * picture files to open as text.
 */

const MODE = "image-viewer";
/** The formats the editor decodes. */
const EXTENSIONS = ["png", "jpg", "jpeg", "gif", "webp", "bmp"];
const ZOOM_STEP = 1.5;
const ZOOM_MAX = 64;

interface Viewer {
  path: string;
  watch: number | null;
  /** 1 shows the whole picture; 2 half its width and height; … */
  zoom: number;
  /** The centre of the part shown, as fractions of the picture. */
  cx: number;
  cy: number;
  /** Opened from an explorer preview and not used since: the next preview replaces it. */
  preview: boolean;
}
const viewers = new Map<number, Viewer>();
/** Files the user asked to see as text (`t`): left as text from then on. */
const asText = new Set<string>();

function basename(path: string): string {
  return path.replace(/[\\/]+$/, "").split(/[\\/]/).pop() ?? path;
}

function dirname(path: string): string {
  const i = Math.max(path.lastIndexOf("/"), path.lastIndexOf("\\"));
  return i > 0 ? path.slice(0, i) : "/";
}

function isImage(path: string): boolean {
  const ext = path.toLowerCase().split(".").pop() ?? "";
  return EXTENSIONS.includes(ext);
}

/** The text under the picture: visible around it when it doesn't fill the pane. */
function caption(v: Viewer): TextPropertyEntry[] {
  return [
    { text: ` ${basename(v.path)}\n`, style: { fg: "ui.help_key_fg", bold: true } },
    {
      text: " +/- zoom  0 fit  hjkl move  r reload  t text  q close\n",
      style: { fg: "editor.line_number_fg" },
    },
  ];
}

/** Tell the editor which part of the picture to show. */
function applyView(id: number, v: Viewer): void {
  const size = 1 / v.zoom;
  v.cx = Math.min(Math.max(v.cx, size / 2), 1 - size / 2);
  v.cy = Math.min(Math.max(v.cy, size / 2), 1 - size / 2);
  editor.setBufferImageView(id, v.cx - size / 2, v.cy - size / 2, size, size);
}

/** Caption, picture and view. A file that can't be read is reported on the status bar. */
function draw(id: number, v: Viewer): void {
  editor.setVirtualBufferContent(id, caption(v));
  editor.setBufferImage(id, v.path, true);
  applyView(id, v);
}

/** Make `id` the viewer of `path`, and watch its folder (a rewrite may replace the file). */
async function attach(id: number, path: string, preview: boolean): Promise<void> {
  const v: Viewer = { path, watch: null, zoom: 1, cx: 0.5, cy: 0.5, preview };
  viewers.set(id, v);
  editor.setBufferShowCursors(id, false);
  draw(id, v);
  try {
    v.watch = await editor.watchPath(dirname(path));
  } catch {
    v.watch = null;
  }
}

/** The viewer left over from the last explorer preview, if any. */
function previewViewer(): number | null {
  for (const [id, v] of viewers) if (v.preview) return id;
  return null;
}

/**
 * Show a viewer of `path` in `splitId` (or the active split): an open one, or
 * a new one. A preview replaces the last preview's viewer.
 */
async function show(path: string, splitId: number | undefined, preview: boolean): Promise<void> {
  for (const [id, v] of viewers) {
    if (v.path === path) {
      if (!preview) v.preview = false;
      if (splitId !== undefined) editor.setSplitBuffer(splitId, id);
      else editor.showBuffer(id);
      return;
    }
  }
  const replaced = preview ? previewViewer() : null;
  const r = await editor.createVirtualBuffer({
    name: basename(path),
    mode: MODE,
    readOnly: true,
    showLineNumbers: false,
    showCursors: false,
    editingDisabled: true,
    ...(splitId !== undefined ? { splitId } : {}),
  });
  await attach(r.bufferId, path, preview);
  if (replaced !== null) editor.closeBuffer(replaced);
}

function active(): [number, Viewer] | null {
  const id = editor.getActiveBufferId();
  const v = viewers.get(id);
  return v ? [id, v] : null;
}

/** Run `f` on the active viewer and redraw its view; using a viewer keeps it. */
function onView(f: (v: Viewer) => void): () => void {
  return () => {
    const a = active();
    if (!a) return;
    const [id, v] = a;
    v.preview = false;
    f(v);
    applyView(id, v);
    editor.setStatus(v.zoom === 1 ? "Image: fit" : `Image: zoom ${Math.round(v.zoom * 100)}%`);
  };
}

/** Move by a quarter of the part shown. */
function pan(dx: number, dy: number): () => void {
  return onView((v) => {
    v.cx += (dx / v.zoom) / 4;
    v.cy += (dy / v.zoom) / 4;
  });
}

registerHandler("image_viewer_zoom_in", onView((v) => {
  v.zoom = Math.min(v.zoom * ZOOM_STEP, ZOOM_MAX);
}));
registerHandler("image_viewer_zoom_out", onView((v) => {
  v.zoom = Math.max(v.zoom / ZOOM_STEP, 1);
}));
registerHandler("image_viewer_fit", onView((v) => {
  v.zoom = 1;
  v.cx = 0.5;
  v.cy = 0.5;
}));
registerHandler("image_viewer_left", pan(-1, 0));
registerHandler("image_viewer_right", pan(1, 0));
registerHandler("image_viewer_up", pan(0, -1));
registerHandler("image_viewer_down", pan(0, 1));
registerHandler("image_viewer_reload", () => {
  const a = active();
  if (a) draw(a[0], a[1]);
});
registerHandler("image_viewer_close", () => {
  const a = active();
  if (a) editor.closeBuffer(a[0]);
});
registerHandler("image_viewer_text", async () => {
  const a = active();
  if (!a) return;
  asText.add(a[1].path);
  await editor.openFile(a[1].path, null, null);
});

editor.defineMode(
  MODE,
  [
    ["+", "image_viewer_zoom_in"],
    ["=", "image_viewer_zoom_in"],
    ["-", "image_viewer_zoom_out"],
    ["0", "image_viewer_fit"],
    ["h", "image_viewer_left"],
    ["Left", "image_viewer_left"],
    ["l", "image_viewer_right"],
    ["Right", "image_viewer_right"],
    ["k", "image_viewer_up"],
    ["Up", "image_viewer_up"],
    ["j", "image_viewer_down"],
    ["Down", "image_viewer_down"],
    ["r", "image_viewer_reload"],
    ["t", "image_viewer_text"],
    ["q", "image_viewer_close"],
  ],
  true,
);
editor.registerCommand("Image: Zoom In", "Show the picture larger", "image_viewer_zoom_in", MODE);
editor.registerCommand("Image: Zoom Out", "Show more of the picture", "image_viewer_zoom_out", MODE);
editor.registerCommand("Image: Fit", "Show the whole picture", "image_viewer_fit", MODE);
editor.registerCommand("Image: Reload", "Load the picture from disk again", "image_viewer_reload", MODE);
editor.registerCommand("Image: Show as Text", "Open the picture's file as text", "image_viewer_text", MODE);

type Openers = {
  registerFileOpener?: (o: { id: string; label: string; extensions: string[]; handler: string }) => void;
};
const openers = editor as unknown as Openers;

if (typeof openers.registerFileOpener === "function") {
  // An editor with file openers hands the viewer a buffer for the file.
  registerHandler("image_viewer_open", async (req: { bufferId: number; path: string }) => {
    await attach(Number(req.bufferId), req.path, false);
  });
  openers.registerFileOpener({
    id: MODE,
    label: "Image Viewer",
    extensions: EXTENSIONS,
    handler: "image_viewer_open",
  });
} else {
  // The editor opened the file as text: put a viewer in its place. An
  // explorer click opens a *preview* tab, which gets no `after_file_open`
  // until it is kept, so `buffer_activated` catches those.
  const swapping = new Set<number>();
  const swap = async (bufferId: number, path: string): Promise<void> => {
    if (!path || !isImage(path) || asText.has(path)) return;
    if (viewers.has(bufferId) || swapping.has(bufferId) || editor.isBufferModified(bufferId)) return;
    if (editor.getImageSupport() === null) return;
    swapping.add(bufferId);
    try {
      const preview = editor.getBufferInfo(bufferId)?.is_preview ?? false;
      const split = editor.listSplits().find((s) => Number(s.bufferId) === bufferId)?.splitId;
      await show(path, split, preview);
      await editor.flush();
      editor.closeBuffer(bufferId);
    } finally {
      swapping.delete(bufferId);
    }
  };
  editor.on("after_file_open", (d) => {
    void swap(d.buffer_id, d.path).catch(() => {});
    return true;
  });
  editor.on("buffer_activated", (d) => {
    const path = editor.getBufferPath(d.buffer_id) ?? "";
    if (isImage(path)) {
      void swap(d.buffer_id, path).catch(() => {});
    } else if (!viewers.has(d.buffer_id) && editor.getBufferInfo(d.buffer_id)?.is_preview) {
      // Browsing went on to another file: the preview viewer goes, as a preview tab would.
      const old = previewViewer();
      if (old !== null) editor.closeBuffer(old);
    }
    return true;
  });
}

editor.on("path_changed", (d) => {
  for (const [id, v] of viewers) if (v.watch === d.handle && d.path === v.path) draw(id, v);
  return true;
});
editor.on("buffer_closed", (d) => {
  const v = viewers.get(d.buffer_id);
  if (!v) return true;
  if (v.watch !== null) editor.unwatchPath(v.watch);
  viewers.delete(d.buffer_id);
  return true;
});
