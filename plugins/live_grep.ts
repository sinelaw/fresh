/// <reference path="./lib/fresh.d.ts" />

/**
 * Live Grep Plugin
 *
 * Project-wide search with ripgrep and live preview.
 * Uses the Finder abstraction for unified search UX.
 *
 * - Type to search across all files
 * - Navigate results with Up/Down to see preview
 * - Press Enter to open file at location
 */

import { Finder, parseGrepOutput } from "./lib/finder.ts";

const editor = getEditor();

// Result type from ripgrep
interface GrepMatch {
  file: string;
  line: number;
  column: number;
  content: string;
}

// Create the finder instance
const finder = new Finder<GrepMatch>(editor, {
  id: "live-grep",
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

// Search function that parses ripgrep output
async function searchWithRipgrep(query: string): Promise<GrepMatch[]> {
  const cwd = editor.getCwd();
  const result = await editor.spawnProcess(
    "rg",
    [
      "--line-number",
      "--column",
      "--no-heading",
      "--color=never",
      "--smart-case",
      "--max-count=100",
      "-g",
      "!.git",
      "-g",
      "!node_modules",
      "-g",
      "!target",
      "-g",
      "!*.lock",
      "--",
      query,
    ],
    cwd
  );

  if (result.exit_code === 0) {
    return parseGrepOutput(result.stdout, 100) as GrepMatch[];
  }
  return [];
}

// Start live grep
globalThis.start_live_grep = function (): void {
  finder.prompt({
    title: editor.t("prompt.live_grep"),
    source: {
      mode: "search",
      search: searchWithRipgrep,
      debounceMs: 150,
      minQueryLength: 2,
    },
  });
};

// Register command
editor.registerCommand(
  "%cmd.live_grep",
  "%cmd.live_grep_desc",
  "start_live_grep",
  "normal"
);

editor.debug("Live Grep plugin loaded (using Finder abstraction)");
editor.setStatus(editor.t("status.ready"));
