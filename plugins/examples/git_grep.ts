/// <reference path="../../types/fresh.d.ts" />

/**
 * Git Grep TypeScript Plugin for Fresh Editor
 *
 * Demonstrates async process spawning with native Promises:
 * - Uses editor.spawnProcess() for async git operations
 * - Parses git grep output and displays results
 * - Shows file opening with line/column positioning
 * - Registers multiple commands on plugin load
 *
 * This is a significant improvement over the Lua version because:
 * - Native async/await instead of callback-based pattern
 * - Cleaner error handling with try/catch
 * - Type safety from TypeScript
 * - Better state management with closures
 */

// Store search results for navigation
interface GrepMatch {
  file: string;
  line: number;
  text: string;
}

let searchResults: GrepMatch[] = [];
let currentResultIndex = 0;

// Parse git grep output into structured results
function parseGitGrepOutput(output: string): GrepMatch[] {
  const matches: GrepMatch[] = [];
  const lines = output.split("\n").filter((line) => line.trim());

  for (const line of lines) {
    // git grep output format: file:line:text
    const match = line.match(/^([^:]+):(\d+):(.*)$/);
    if (match) {
      matches.push({
        file: match[1],
        line: parseInt(match[2], 10),
        text: match[3].trim(),
      });
    }
  }

  return matches;
}

// Action: Search for pattern in repository
globalThis.git_grep_search = async function (): Promise<void> {
  // For now, search for a hardcoded pattern
  // In a full implementation, this would use a prompt
  const pattern = "TODO";

  editor.setStatus(`Searching for "${pattern}"...`);

  try {
    const result = await editor.spawnProcess("git", [
      "grep",
      "-n", // Show line numbers
      "-I", // Skip binary files
      pattern,
    ]);

    if (result.exit_code === 0 && result.stdout.trim()) {
      searchResults = parseGitGrepOutput(result.stdout);
      currentResultIndex = 0;

      if (searchResults.length > 0) {
        editor.setStatus(
          `Found ${searchResults.length} matches for "${pattern}"`
        );
        editor.debug(`Git grep results: ${JSON.stringify(searchResults)}`);

        // Jump to first result
        const first = searchResults[0];
        editor.openFile(first.file, first.line, 1);
        editor.setStatus(
          `[1/${searchResults.length}] ${first.file}:${first.line}: ${first.text}`
        );
      }
    } else if (result.exit_code === 1) {
      // git grep returns 1 when no matches found
      editor.setStatus(`No matches found for "${pattern}"`);
      searchResults = [];
    } else {
      editor.setStatus(`Git grep error: ${result.stderr || "Unknown error"}`);
      editor.debug(`Git grep failed with exit code ${result.exit_code}`);
    }
  } catch (error) {
    editor.setStatus(`Git grep failed: ${error}`);
    editor.debug(`Git grep exception: ${error}`);
  }
};

// Action: Go to next search result
globalThis.git_grep_next = function (): void {
  if (searchResults.length === 0) {
    editor.setStatus("No search results. Run git_grep_search first.");
    return;
  }

  currentResultIndex = (currentResultIndex + 1) % searchResults.length;
  const result = searchResults[currentResultIndex];

  editor.openFile(result.file, result.line, 1);
  editor.setStatus(
    `[${currentResultIndex + 1}/${searchResults.length}] ${result.file}:${result.line}: ${result.text}`
  );
};

// Action: Go to previous search result
globalThis.git_grep_prev = function (): void {
  if (searchResults.length === 0) {
    editor.setStatus("No search results. Run git_grep_search first.");
    return;
  }

  currentResultIndex =
    (currentResultIndex - 1 + searchResults.length) % searchResults.length;
  const result = searchResults[currentResultIndex];

  editor.openFile(result.file, result.line, 1);
  editor.setStatus(
    `[${currentResultIndex + 1}/${searchResults.length}] ${result.file}:${result.line}: ${result.text}`
  );
};

// Action: Show current git status
globalThis.git_status = async function (): Promise<void> {
  editor.setStatus("Getting git status...");

  try {
    const result = await editor.spawnProcess("git", ["status", "--short"]);

    if (result.exit_code === 0) {
      const lines = result.stdout.trim().split("\n").filter((l) => l);
      if (lines.length === 0) {
        editor.setStatus("Git: Clean working directory");
      } else {
        editor.setStatus(`Git: ${lines.length} changed file(s)`);
        editor.debug(`Git status:\n${result.stdout}`);
      }
    } else {
      editor.setStatus(`Not a git repository or git error`);
    }
  } catch (error) {
    editor.setStatus(`Git status failed: ${error}`);
  }
};

// Action: Show current branch
globalThis.git_branch = async function (): Promise<void> {
  try {
    const result = await editor.spawnProcess("git", [
      "rev-parse",
      "--abbrev-ref",
      "HEAD",
    ]);

    if (result.exit_code === 0) {
      const branch = result.stdout.trim();
      editor.setStatus(`Git branch: ${branch}`);
    } else {
      editor.setStatus("Not a git repository");
    }
  } catch (error) {
    editor.setStatus(`Git branch failed: ${error}`);
  }
};

// Action: Show recent commits
globalThis.git_log = async function (): Promise<void> {
  editor.setStatus("Fetching recent commits...");

  try {
    const result = await editor.spawnProcess("git", [
      "log",
      "--oneline",
      "-10", // Last 10 commits
    ]);

    if (result.exit_code === 0) {
      const lines = result.stdout.trim().split("\n");
      editor.setStatus(`Git: ${lines.length} recent commits`);
      editor.debug(`Recent commits:\n${result.stdout}`);

      // Show first commit in status
      if (lines.length > 0) {
        editor.setStatus(`Latest: ${lines[0]}`);
      }
    } else {
      editor.setStatus("Git log failed");
    }
  } catch (error) {
    editor.setStatus(`Git log failed: ${error}`);
  }
};

// Register commands on plugin load
editor.registerCommand(
  "Git Grep: Search TODOs",
  "Search for TODO comments in the repository",
  "git_grep_search",
  "normal"
);

editor.registerCommand(
  "Git Grep: Next Result",
  "Jump to next search result",
  "git_grep_next",
  "normal"
);

editor.registerCommand(
  "Git Grep: Previous Result",
  "Jump to previous search result",
  "git_grep_prev",
  "normal"
);

editor.registerCommand(
  "Git: Show Status",
  "Display git status summary",
  "git_status",
  "" // Available in all contexts
);

editor.registerCommand(
  "Git: Show Branch",
  "Display current git branch",
  "git_branch",
  ""
);

editor.registerCommand(
  "Git: Recent Commits",
  "Show recent commit history",
  "git_log",
  ""
);

// Plugin initialized
editor.setStatus("Git Grep plugin loaded - 6 commands registered");
editor.debug("Git Grep TypeScript plugin initialized");

// Automatically show git branch on load
(async () => {
  try {
    const result = await editor.spawnProcess("git", [
      "rev-parse",
      "--abbrev-ref",
      "HEAD",
    ]);
    if (result.exit_code === 0) {
      const branch = result.stdout.trim();
      editor.setStatus(`Git Grep plugin ready | Branch: ${branch}`);
    }
  } catch {
    // Silently fail if not in a git repo
  }
})();
