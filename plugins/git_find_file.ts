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
    return result.stdout.split("\n").filter((line) => line.trim() !== "");
  }

  editor.debug(`Failed to load git files: ${result.stderr}`);
  return [];
}

// Global function to start file finder
globalThis.start_git_find_file = function (): void {
  finder.prompt({
    title: editor.t("prompt.find_file"),
    source: {
      mode: "filter",
      load: loadGitFiles,
      // Uses built-in fuzzy filter by default
    },
  });
};

// Reload git files command
globalThis.git_reload_files = async function (): Promise<void> {
  // Just re-trigger the prompt which will reload
  globalThis.start_git_find_file();
  editor.setStatus(editor.t("status.reloading"));
};

// Register commands
editor.registerCommand(
  "%cmd.find",
  "%cmd.find_desc",
  "start_git_find_file",
  "normal"
);

editor.registerCommand(
  "%cmd.reload",
  "%cmd.reload_desc",
  "git_reload_files",
  "normal"
);

editor.debug("Git Find File plugin loaded (using Finder abstraction)");
editor.setStatus(editor.t("status.ready"));
