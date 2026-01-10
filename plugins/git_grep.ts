/// <reference path="./lib/fresh.d.ts" />
import {
  SearchMatch,
  SearchPreview,
  DebouncedSearch,
  parseGrepLine,
  matchesToSuggestions,
} from "./lib/search-utils.ts";

const editor = getEditor();

/**
 * Git Grep Plugin
 *
 * Provides interactive git grep functionality with live search results
 * and preview panel (matching Live Grep's UX).
 */

// State management
let gitGrepResults: SearchMatch[] = [];
const preview = new SearchPreview(editor, "git-grep-preview");
const search = new DebouncedSearch(editor, { debounceMs: 150, minQueryLength: 1 });

// Parse git grep output into matches
function parseGitGrepOutput(stdout: string): SearchMatch[] {
  const results: SearchMatch[] = [];

  for (const line of stdout.split("\n")) {
    if (!line.trim()) continue;
    const match = parseGrepLine(line);
    if (match) {
      results.push(match);
      // Limit to 100 results for performance
      if (results.length >= 100) {
        break;
      }
    }
  }

  return results;
}

// Global function to start git grep
globalThis.start_git_grep = function (): void {
  // Clear previous results
  gitGrepResults = [];
  search.reset();

  // Remember original split for preview
  preview.setOriginalSplit(editor.getActiveSplitId());

  // Start the prompt
  editor.startPrompt(editor.t("prompt.grep"), "git-grep");
  editor.setStatus(editor.t("status.type_to_search"));
};

// React to prompt input changes (with debouncing)
globalThis.onGitGrepPromptChanged = function (args: {
  prompt_type: string;
  input: string;
}): boolean {
  if (args.prompt_type !== "git-grep") {
    return true;
  }

  const query = args.input;

  // Don't search for empty queries
  if (!query || query.trim() === "") {
    editor.setPromptSuggestions([]);
    gitGrepResults = [];
    return true;
  }

  // Run debounced search
  const cwd = editor.getCwd();
  search.search(
    query,
    () => editor.spawnProcess("git", ["grep", "-n", "--column", "-I", "--", query], cwd),
    (result) => {
      if (result.exit_code === 0) {
        gitGrepResults = parseGitGrepOutput(result.stdout);
        editor.setPromptSuggestions(matchesToSuggestions(gitGrepResults));

        if (gitGrepResults.length > 0) {
          editor.setStatus(editor.t("status.found", { count: String(gitGrepResults.length) }));
          // Show preview of first result
          preview.update(gitGrepResults[0]);
        } else {
          editor.setStatus(editor.t("status.no_matches"));
        }
      } else if (result.exit_code === 1) {
        // No matches found (git grep returns 1)
        gitGrepResults = [];
        editor.setPromptSuggestions([]);
        editor.setStatus(editor.t("status.no_matches"));
      } else if (result.exit_code !== -1) {
        // Error occurred (ignore -1 which means killed)
        editor.setStatus(editor.t("status.error", { error: result.stderr }));
      }
    }
  );

  return true;
};

// Handle selection changes - update preview
globalThis.onGitGrepSelectionChanged = function (args: {
  prompt_type: string;
  selected_index: number;
}): boolean {
  if (args.prompt_type !== "git-grep") {
    return true;
  }

  const match = gitGrepResults[args.selected_index];
  if (match) {
    preview.update(match);
  }

  return true;
};

// Handle prompt confirmation (user pressed Enter)
globalThis.onGitGrepPromptConfirmed = function (args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): boolean {
  if (args.prompt_type !== "git-grep") {
    return true;
  }

  // Cancel any running search
  search.cancel();

  // Close preview
  preview.close();

  // Check if user selected a suggestion
  if (args.selected_index !== null && gitGrepResults[args.selected_index]) {
    const selected = gitGrepResults[args.selected_index];
    editor.openFile(selected.file, selected.line, selected.column);
    editor.setStatus(
      editor.t("status.opened", {
        location: `${selected.file}:${selected.line}:${selected.column}`,
      })
    );
  } else {
    editor.setStatus(editor.t("status.no_selection"));
  }

  // Clear state
  gitGrepResults = [];

  return true;
};

// Handle prompt cancellation (user pressed Escape)
globalThis.onGitGrepPromptCancelled = function (args: {
  prompt_type: string;
}): boolean {
  if (args.prompt_type !== "git-grep") {
    return true;
  }

  // Cancel search and close preview
  search.cancel();
  preview.close();

  // Clear results
  gitGrepResults = [];
  editor.setStatus(editor.t("status.cancelled"));

  return true;
};

// Register event handlers
editor.on("prompt_changed", "onGitGrepPromptChanged");
editor.on("prompt_selection_changed", "onGitGrepSelectionChanged");
editor.on("prompt_confirmed", "onGitGrepPromptConfirmed");
editor.on("prompt_cancelled", "onGitGrepPromptCancelled");

// Register command
editor.registerCommand("%cmd.grep", "%cmd.grep_desc", "start_git_grep", "normal");

// Log that plugin loaded successfully
editor.debug("Git Grep plugin loaded (with preview support)");
editor.setStatus(editor.t("status.ready"));
