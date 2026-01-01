/// <reference path="../types/fresh.d.ts" />

/**
 * Git Grep Plugin
 *
 * Provides interactive git grep functionality with live search results.
 */

interface GrepMatch {
  file: string;
  line: number;
  column: number;
  content: string;
}

// State management
let gitGrepResults: GrepMatch[] = [];

// Parse git grep output line
// Format: file:line:column:content
function parseGitGrepLine(line: string): GrepMatch | null {
  const match = line.match(/^([^:]+):(\d+):(\d+):(.*)$/);
  if (match) {
    return {
      file: match[1],
      line: parseInt(match[2], 10),
      column: parseInt(match[3], 10),
      content: match[4].trimStart(),
    };
  }
  return null;
}

// Parse git grep output into suggestions
function parseGitGrepOutput(stdout: string): {
  results: GrepMatch[];
  suggestions: PromptSuggestion[];
} {
  const results: GrepMatch[] = [];
  const suggestions: PromptSuggestion[] = [];

  for (const line of stdout.split("\n")) {
    if (!line.trim()) continue;
    const match = parseGitGrepLine(line);
    if (match) {
      results.push(match);
      suggestions.push({
        text: `${match.file}:${match.line}:${match.column}`,
        description: match.content,
        value: `${match.file}:${match.line}:${match.column}`,
        disabled: false,
      });

      // Limit to 100 results for performance
      if (results.length >= 100) {
        break;
      }
    }
  }

  return { results, suggestions };
}

// Global function to start git grep
globalThis.start_git_grep = function(): void {
  // Clear previous results
  gitGrepResults = [];

  // Start the prompt
  editor.startPrompt(editor.t("prompt.grep"), "git-grep");
  editor.setStatus(editor.t("status.type_to_search"));
};

// React to prompt input changes
globalThis.onGitGrepPromptChanged = function(args: {
  prompt_type: string;
  input: string;
}): boolean {
  if (args.prompt_type !== "git-grep") {
    return true; // Not our prompt
  }

  const query = args.input;

  // Don't search for empty queries
  if (!query || query.trim() === "") {
    editor.setPromptSuggestions([]);
    return true;
  }

  // Spawn git grep asynchronously
  const cwd = editor.getCwd();
  editor.spawnProcess("git", ["grep", "-n", "--column", "-I", "--", query], cwd)
    .then((result) => {
      if (result.exit_code === 0) {
        // Parse results and update suggestions
        const { results, suggestions } = parseGitGrepOutput(result.stdout);
        gitGrepResults = results;

        // Update prompt with suggestions
        editor.setPromptSuggestions(suggestions);

        // Update status
        if (results.length > 0) {
          editor.setStatus(editor.t("status.found", { count: String(results.length) }));
        } else {
          editor.setStatus(editor.t("status.no_matches"));
        }
      } else if (result.exit_code === 1) {
        // No matches found (git grep returns 1)
        gitGrepResults = [];
        editor.setPromptSuggestions([]);
        editor.setStatus(editor.t("status.no_matches"));
      } else {
        // Error occurred
        editor.setStatus(editor.t("status.error", { error: result.stderr }));
      }
    })
    .catch((e) => {
      editor.setStatus(editor.t("status.error", { error: String(e) }));
    });

  return true;
};

// Handle prompt confirmation (user pressed Enter)
globalThis.onGitGrepPromptConfirmed = function(args: {
  prompt_type: string;
  selected_index: number | null;
  input: string;
}): boolean {
  if (args.prompt_type !== "git-grep") {
    return true; // Not our prompt
  }

  editor.debug(
    `prompt-confirmed: selected_index=${args.selected_index}, num_results=${gitGrepResults.length}`
  );

  // Check if user selected a suggestion
  if (args.selected_index !== null && gitGrepResults[args.selected_index]) {
    const selected = gitGrepResults[args.selected_index];

    editor.debug(`Opening file: ${selected.file}:${selected.line}:${selected.column}`);

    // Open the file at the specific location
    editor.openFile(selected.file, selected.line, selected.column);
    editor.setStatus(editor.t("status.opened", { location: `${selected.file}:${selected.line}:${selected.column}` }));
  } else {
    // No selection
    editor.debug("No file selected - selected_index is null or out of bounds");
    editor.setStatus(editor.t("status.no_selection"));
  }

  return true;
};

// Handle prompt cancellation (user pressed Escape)
globalThis.onGitGrepPromptCancelled = function(args: {
  prompt_type: string;
}): boolean {
  if (args.prompt_type !== "git-grep") {
    return true; // Not our prompt
  }

  // Clear results
  gitGrepResults = [];
  editor.setStatus(editor.t("status.cancelled"));

  return true;
};

// Register event handlers
editor.on("prompt_changed", "onGitGrepPromptChanged");
editor.on("prompt_confirmed", "onGitGrepPromptConfirmed");
editor.on("prompt_cancelled", "onGitGrepPromptCancelled");

// Register command
editor.registerCommand(
  "%cmd.grep",
  "%cmd.grep_desc",
  "start_git_grep",
  "normal"
);

// Log that plugin loaded successfully
editor.debug("Git Grep plugin loaded successfully (TypeScript)");
editor.debug("Usage: Call start_git_grep() or use command palette 'Git Grep'");
editor.setStatus(editor.t("status.ready"));
