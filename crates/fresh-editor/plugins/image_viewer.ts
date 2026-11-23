/// <reference path="./lib/fresh.d.ts" />
const editor = getEditor();

/**
 * Image Viewer
 *
 * Shows a picture file (PNG, JPEG, GIF, WebP, BMP) as a picture, in the
 * file's own tab: the tab stays the file's tab in every way (`createBufferGroup`
 * with `inTabOf`), and only what it shows changes. The editor draws the
 * picture (`setBufferImage`) with the terminal's own graphics where it has
 * them (Kitty, Sixel, iTerm2) and coloured half blocks everywhere else,
 * fitted to the pane.
 *
 * A picture file shows as a picture when it opens. "Show as Text" (`t`)
 * goes back to the file's text, and "View as Image" (the tab's right-click
 * menu, or the palette) to the picture.
 *
 * Keys in the viewer:
 *   + / =   zoom in          -   zoom out         0   fit the pane
 *   h j k l or arrows: move around a zoomed picture
 *   r   load the file again  t   show the file as text   q   close the tab
 *
 * It redraws when the file changes on disk (a plot a script rewrites shows
 * the new one), for files on this computer.
 *
 * A build without image support (`getImageSupport()` is null) leaves
 * picture files as text.
 */

const MODE = "image-viewer";
/** The formats the editor decodes. */
const EXTENSIONS = ["png", "jpg", "jpeg", "gif", "webp", "bmp"];
const ZOOM_STEP = 1.5;
const ZOOM_MAX = 64;

interface Viewer {
  /** The file's own buffer, whose tab shows the picture. */
  fileBuffer: number;
  windowId: number;
  groupId: number;
  /** The panel above the picture, with the file's name and the keys. */
  captionPanel: number;
  /** The picture's file, on its window's filesystem. */
  path: WindowPath;
  watch: number | null;
  /** 1 shows the whole picture; 2 half its width and height; … */
  zoom: number;
  /** The centre of the part shown, as fractions of the picture. */
  cx: number;
  cy: number;
}
/** Viewers by the buffer of their panel (the buffer in focus while one is shown). */
const viewers = new Map<number, Viewer>();
/** File buffers the user asked to see as text (`t`): left as text from then on. */
const asText = new Set<number>();
/** File buffers a viewer is being made for. */
const opening = new Set<number>();

function basename(path: string): string {
  return editor.pathBasename(path) || path;
}

function isImage(path: string): boolean {
  const ext = path.toLowerCase().split(".").pop() ?? "";
  return EXTENSIONS.includes(ext);
}

function viewerOf(fileBuffer: number): [number, Viewer] | null {
  for (const [panel, v] of viewers) if (v.fileBuffer === fileBuffer) return [panel, v];
  return null;
}

/** The caption above the picture: the file's name and the keys. */
function caption(v: Viewer): TextPropertyEntry[] {
  return [
    { text: ` ${basename(v.path.value)}\n`, style: { fg: "ui.help_key_fg", bold: true } },
    {
      text: " +/- zoom  0 fit  hjkl move  r reload  t text  q close\n",
      style: { fg: "editor.line_number_fg" },
    },
  ];
}

/** Tell the editor which part of the picture to show. */
function applyView(panel: number, v: Viewer): void {
  const size = 1 / v.zoom;
  v.cx = Math.min(Math.max(v.cx, size / 2), 1 - size / 2);
  v.cy = Math.min(Math.max(v.cy, size / 2), 1 - size / 2);
  editor.setBufferImageView(panel, v.cx - size / 2, v.cy - size / 2, size, size);
}

/** Caption, picture and view. */
function draw(panel: number, v: Viewer): void {
  editor.setVirtualBufferContent(v.captionPanel, caption(v));
  if (!editor.setBufferImage(panel, v.path, true)) {
    editor.setStatus(`Image: cannot read ${basename(v.path.value)}`);
  }
  applyView(panel, v);
}

/** Whether a window's files are on this computer, where a watch can see them. */
function isLocalWindow(windowId: number): boolean {
  const w = editor.listWindows().find((w) => w.id === windowId);
  return w === undefined || !w.remote;
}

/**
 * Show the file of `fileBuffer` as a picture in its own tab. Nothing happens
 * for a file that already shows one, or has unsaved changes.
 */
async function view(fileBuffer: number): Promise<void> {
  if (opening.has(fileBuffer) || viewerOf(fileBuffer) !== null) return;
  const info = editor.getBufferInfo(fileBuffer);
  if (!info || !info.path || info.modified) return;
  opening.add(fileBuffer);
  try {
    const group = await editor.createBufferGroup(
      basename(info.path),
      MODE,
      JSON.stringify({
        type: "split",
        direction: "v",
        ratio: 0.1,
        first: { type: "fixed", id: "caption", height: 2 },
        second: { type: "scrollable", id: "picture" },
      }),
      { inTabOf: fileBuffer },
    );
    const panel = group.panels.picture;
    const v: Viewer = {
      fileBuffer,
      windowId: info.window_id,
      groupId: group.groupId,
      captionPanel: group.panels.caption,
      path: editor.windowPath(info.window_id, info.path),
      watch: null,
      zoom: 1,
      cx: 0.5,
      cy: 0.5,
    };
    viewers.set(panel, v);
    editor.focusBufferGroupPanel(group.groupId, "picture");
    draw(panel, v);
    // A plot a script rewrites: watch the folder, as a rewrite may replace
    // the file. Watches see this computer's files only.
    if (isLocalWindow(v.windowId)) {
      try {
        v.watch = await editor.watchPath(editor.pathDirname(info.path));
      } catch {
        v.watch = null;
      }
    }
  } finally {
    opening.delete(fileBuffer);
  }
}

/** Show a file as a picture when it opens, unless the user wants it as text. */
function viewOnOpen(fileBuffer: number, path: string): void {
  if (!path || !isImage(path) || asText.has(fileBuffer)) return;
  if (editor.getImageSupport() === null) return;
  void view(fileBuffer).catch((e) => editor.error(`image_viewer: ${e}`));
}

function active(): [number, Viewer] | null {
  const panel = editor.getActiveBufferId();
  const v = viewers.get(panel);
  return v ? [panel, v] : null;
}

/** Run `f` on the active viewer and redraw its view. */
function onView(f: (v: Viewer) => void): () => void {
  return () => {
    const a = active();
    if (!a) return;
    const [panel, v] = a;
    f(v);
    applyView(panel, v);
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
  if (a) editor.closeBuffer(a[1].fileBuffer);
});
/** Back to the file's text: closing the group shows the file again. */
registerHandler("image_viewer_text", () => {
  const a = active();
  if (!a) return;
  asText.add(a[1].fileBuffer);
  editor.closeBufferGroup(a[1].groupId);
});

/** "View as Image" on a file's buffer, from the tab menu or the palette. */
function viewAsImage(fileBuffer: number): void {
  const path = editor.getBufferPath(fileBuffer);
  if (!path) {
    editor.setStatus("Image: this tab has no file");
    return;
  }
  if (editor.getImageSupport() === null) {
    editor.setStatus("Image: this build cannot show pictures");
    return;
  }
  asText.delete(fileBuffer);
  void view(fileBuffer).catch((e) => editor.error(`image_viewer: ${e}`));
}
registerHandler("image_viewer_view_tab", (target: TabMenuTarget) => {
  viewAsImage(target.bufferId);
});
registerHandler("image_viewer_view_active", () => {
  viewAsImage(editor.getActiveBufferId());
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
editor.registerCommand("Image: Show as Text", "Show the picture's file as text", "image_viewer_text", MODE);
editor.registerCommand("Image: View as Image", "Show the active file as a picture", "image_viewer_view_active", null);
editor.addTabMenuItem({ label: "View as Image", scope: "fileTab", handler: "image_viewer_view_tab" });

// A picture shows as a picture when it opens. An explorer click opens a
// *preview* tab, which gets no `after_file_open` until it is kept, so
// `buffer_activated` catches those; the viewer goes with the preview tab.
editor.on("after_file_open", (d) => {
  viewOnOpen(d.buffer_id, d.path);
  return true;
});
editor.on("buffer_activated", (d) => {
  if (!viewers.has(d.buffer_id)) viewOnOpen(d.buffer_id, editor.getBufferPath(d.buffer_id) ?? "");
  return true;
});

editor.on("path_changed", (d) => {
  for (const [panel, v] of viewers) if (v.watch === d.handle && d.path === v.path.value) draw(panel, v);
  return true;
});
editor.on("buffer_group_closed", (d) => {
  for (const [panel, v] of viewers) {
    if (v.groupId !== d.group_id || v.windowId !== d.window_id) continue;
    if (v.watch !== null) editor.unwatchPath(v.watch);
    viewers.delete(panel);
  }
  return true;
});
editor.on("buffer_closed", (d) => {
  asText.delete(d.buffer_id);
  return true;
});
