/// <reference path="./lib/fresh.d.ts" />

/**
 * Git Find File Plugin
 *
 * Provides interactive file finding functionality with fuzzy search
 * for git-tracked files. Uses the Finder abstraction with filter mode.
 */

import { Finder } from "./lib/finder.ts";
import { resolveGitRepo, toAbsInRepo } from "./lib/git_repo.ts";

const editor = getEditor();

// One git-tracked file: `rel` (repo-relative) is what the user sees and
// fuzzy-matches against; `abs` is what actually opens, because in a monorepo
// the workspace root can differ from the repo root, so a relative path
// wouldn't resolve.
type GitFile = { rel: string; abs: string };

// Create the finder instance with filter mode
const finder = new Finder<GitFile>(editor, {
  id: "git-find-file",
  format: (file) => ({
    label: file.rel,
    location: { file: file.abs, line: 1, column: 1 },
  }),
  preview: false, // No preview for file finder
  maxResults: 100,
});

// Load git-tracked files
async function loadGitFiles(): Promise<GitFile[]> {
  // Resolve the repo from the active buffer's dir too, so this works from a
  // sub-project buffer even when the workspace root isn't itself a repo.
  const repo = await resolveGitRepo(editor);
  if (!repo) {
    editor.debug("git-find-file: not inside a git repository");
    return [];
  }

  const result = await editor.spawnProcess(
    "git",
    ["ls-files", "--full-name"],
    repo.root,
  );
  if (result.exit_code !== 0) {
    editor.debug(`Failed to load git files: ${result.stderr}`);
    return [];
  }

  // `ls-files --full-name` yields repo-relative paths. Keep them for display
  // and join to the repo root for opening.
  return result.stdout
    .split("\n")
    .map((line) => line.trim())
    .filter((line) => line !== "")
    .map((rel) => ({ rel, abs: toAbsInRepo(editor, repo, rel) }));
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
