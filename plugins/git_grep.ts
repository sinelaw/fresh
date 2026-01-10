/// <reference path="./lib/fresh.d.ts" />

/**
 * Git Grep Plugin
 *
 * Provides interactive git grep functionality with live search results
 * and preview panel. Uses the Finder abstraction for unified search UX.
 */

import { Finder, parseGrepOutput } from "./lib/finder.ts";

const editor = getEditor();

// Result type from git grep
interface GrepMatch {
  file: string;
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
      file: match.file,
      line: match.line,
      column: match.column,
    },
  }),
  preview: true,
  maxResults: 100,
});

// Search function using git grep
async function searchWithGitGrep(query: string): Promise<GrepMatch[]> {
  const cwd = editor.getCwd();
  const result = await editor.spawnProcess(
    "git",
    ["grep", "-n", "--column", "-I", "--", query],
    cwd
  );

  if (result.exit_code === 0) {
    return parseGrepOutput(result.stdout, 100) as GrepMatch[];
  }
  return [];
}

// Global function to start git grep
globalThis.start_git_grep = function (): void {
  finder.prompt({
    title: editor.t("prompt.grep"),
    source: {
      mode: "search",
      search: searchWithGitGrep,
      debounceMs: 150,
      minQueryLength: 1,
    },
  });
};

// Register command
editor.registerCommand("%cmd.grep", "%cmd.grep_desc", "start_git_grep", "normal");

// Log that plugin loaded successfully
editor.debug("Git Grep plugin loaded (using Finder abstraction)");
editor.setStatus(editor.t("status.ready"));
