/// <reference path="./lib/fresh.d.ts" />

/**
 * Git Find File Plugin
 *
 * Provides interactive file finding functionality with fuzzy search
 * for git-tracked files. Uses the Finder abstraction with filter mode.
 */

import { Finder } from "./lib/finder.ts";

const editor = getEditor();

// Create the finder instance with filter mode
const finder = new Finder<string>(editor, {
  id: "git-find-file",
  format: (file) => ({
    label: file,
    location: { file, line: 1, column: 1 },
  }),
  preview: false, // No preview for file finder
  maxResults: 100,
});

// Load git-tracked files
async function loadGitFiles(): Promise<string[]> {
  const result = await editor.spawnProcess("git", ["ls-files"]);

  if (result.exit_code === 0) {
    // Split by newline and trim each line to handle \r\n on Windows
    return result.stdout
      .split("\n")
      .map((line) => line.trim())
      .filter((line) => line !== "");
  }

  editor.debug(`Failed to load git files: ${result.stderr}`);
  return [];
}

// Global function to start file finder
function start_git_find_file() : void {
  finder.prompt({
    title: editor.t("prompt.find_file"),
    source: {
      mode: "filter",
      load: loadGitFiles,
      // Uses built-in fuzzy filter by default
    },
  });
}
registerHandler("start_git_find_file", start_git_find_file);

// Reload git files command
async function git_reload_files() : Promise<void> {
  // Just re-trigger the prompt which will reload
  start_git_find_file();
  editor.setStatus(editor.t("status.reloading"));
}
registerHandler("git_reload_files", git_reload_files);

// Register commands
editor.registerCommand(
  "%cmd.find",
  "%cmd.find_desc",
  "start_git_find_file",
  null
);

editor.registerCommand(
  "%cmd.reload",
  "%cmd.reload_desc",
  "git_reload_files",
  null
);

editor.debug("Git Find File plugin loaded (using Finder abstraction)");
