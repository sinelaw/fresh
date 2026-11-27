// The built-in plugins, in the order they are activated. Each is imported
// statically, so the whole client is one module graph the browser loads
// (and a checker reads) up front: no module URL is ever built at run time.
// Adding a plugin is adding its import and its line here.

import * as welcome from "./welcome.js";
import * as explorer from "./explorer.js";
import * as search from "./search.js";
import * as lsp from "./lsp.js";
import * as git from "./git.js";
import * as markdown from "./markdown.js";
import * as compare from "./compare.js";
import * as tours from "./tours.js";
import * as keybindings from "./keybindings.js";
import * as themes from "./themes.js";
import * as bookmarks from "./bookmarks.js";

export const PLUGINS = [
  { name: "welcome", activate: welcome.activate },
  { name: "explorer", activate: explorer.activate },
  { name: "search", activate: search.activate },
  { name: "lsp", activate: lsp.activate },
  { name: "git", activate: git.activate },
  { name: "markdown", activate: markdown.activate },
  { name: "compare", activate: compare.activate },
  { name: "tours", activate: tours.activate },
  { name: "keybindings", activate: keybindings.activate },
  { name: "themes", activate: themes.activate },
  { name: "bookmarks", activate: bookmarks.activate },
];
