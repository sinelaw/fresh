/// <reference path="./lib/fresh.d.ts" />
//
// Test-only viewer for the bundled Open With plugin. It shows `.fakeimg`
// files as one line naming the viewer and the file, so a test can tell from
// the screen which view a file is in. The type import makes Open With load
// first, so its API is there below.
import type { OpenWithApi } from "fresh:plugin/open_with";

const editor = getEditor();

const openWith = editor.getPluginApi("open-with") as OpenWithApi | null;
if (!openWith) throw new Error("test_open_with_viewer: open-with is not loaded");

openWith.registerViewer({
  id: "fake-viewer",
  label: "Fake Viewer",
  extensions: ["fakeimg"],
  open: (req) => {
    editor.setVirtualBufferContent(req.bufferId, [
      { text: `VIEWER ${req.openerId} ${editor.pathBasename(req.path.value)}\n` },
    ]);
  },
});

// Last, so a test that sees it knows Open With's menu items are in place:
// the editor applies plugin commands in the order they were sent.
editor.setStatus("fake viewer ready");
