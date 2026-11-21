/// <reference path="./lib/fresh.d.ts" />
//
// Test-only plugin for `registerFileOpener`. It shows `.fakeimg` files (a
// default opener) and `.fakeopt` files (an opener the user has to pick in
// the `file_openers` setting) as one line naming the opener and the file,
// so a test can tell from the screen which view a file opened in.
const editor = getEditor();

globalThis.test_file_opener_show = (req: FileOpenRequest) => {
  const name = req.path.split(/[\\/]/).pop();
  editor.setVirtualBufferContent(req.bufferId, [
    { text: `VIEWER ${req.openerId} ${name}\n` },
  ]);
};

editor.registerFileOpener({
  id: "fake-viewer",
  label: "Fake Viewer",
  extensions: ["fakeimg"],
  handler: "test_file_opener_show",
});

editor.registerFileOpener({
  id: "fake-option",
  label: "Fake Option",
  extensions: ["fakeopt"],
  priority: "option",
  handler: "test_file_opener_show",
});

editor.debug("test_file_opener: loaded");
