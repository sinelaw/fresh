/// <reference path="./lib/fresh.d.ts" />

/**
 * Git Grep Plugin
 *
 * Provides interactive git grep functionality with live search results
 * and preview panel. Uses the Finder abstraction for unified search UX.
 */

import { Finder, parseGrepOutput } from "./lib/finder.ts";
import { git, resolveGitRepo, toAbsInRepo } from "./lib/git_repo.ts";

const editor = getEditor();

// One git-grep hit. `file` is repo-relative (what git prints and what the
// user sees); `abs` is the absolute path we actually open, because in a
// monorepo the repo root differs from the workspace cwd, so a repo-relative
// path wouldn't resolve.
interface GrepMatch {
  file: string;
  abs: string;
  line: number;
  column: number;
  content: string;
}

// Create the finder instance
const finder = new Finder<GrepMatch>(editor, {
  id: "git-grep",
  format: (match) => ({
    label: `${match.file}:${match.line}`,
    description:
      match.content.length > 60
        ? match.content.substring(0, 57).trim() + "..."
        : match.content.trim(),
    location: {
      file: match.abs,
      line: match.line,
      column: match.column,
    },
  }),
  preview: true,
  maxResults: 100,
});

// Search function using git grep. Resolves the repo from the active buffer
// (so it works from a sub-project buffer even when the workspace root isn't a
// repo) and runs git inside it via the shared gateway.
async function searchWithGitGrep(query: string): Promise<GrepMatch[]> {
  const repo = await resolveGitRepo(editor);
  if (!repo) {
    editor.setStatus(editor.t("status.not_in_git"));
    return [];
  }

  const result = await git(editor, repo, [
    "grep",
    "-n",
    "--column",
    "-I",
    "--",
    query,
  ]);

  if (result.exit_code === 0) {
    const matches = parseGrepOutput(
      result.stdout,
      100,
      (msg) => editor.debug(msg),
    ) as GrepMatch[];
    // `git grep` prints repo-relative paths; join each onto the repo root so
    // selecting a result opens the right file regardless of the workspace cwd.
    return matches.map((m) => ({ ...m, abs: toAbsInRepo(editor, repo, m.file) }));
  }
  editor.error(`[git_grep] process exited with code ${result.exit_code}: ${result.stderr}`);
  editor.setStatus(`git grep failed (exit ${result.exit_code})`);
  return [];
}

// Global function to start git grep
function start_git_grep() : void {
  finder.prompt({
    title: editor.t("prompt.grep"),
    source: {
      mode: "search",
      search: searchWithGitGrep,
      debounceMs: 150,
      minQueryLength: 1,
    },
  });
}
registerHandler("start_git_grep", start_git_grep);

// Register command
editor.registerCommand("%cmd.grep", "%cmd.grep_desc", "start_git_grep", null);

// Log that plugin loaded successfully
editor.debug("Git Grep plugin loaded (using Finder abstraction)");
