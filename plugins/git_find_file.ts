/// <reference path="../types/fresh.d.ts" />

/**
 * Git Find File Plugin
 *
 * Provides git file finding functionality with fuzzy search.
 *
 * NOTE: This is a simplified version that uses the available TypeScript API.
 * The full interactive prompt UI (set_prompt_suggestions, etc.) is not yet
 * available in the TypeScript runtime. This version loads files and provides
 * basic search commands.
 */

// State management
let allFiles: string[] = [];
let isLoading = false;

// Simple fuzzy filter function
function fuzzyMatch(str: string, pattern: string): boolean {
  if (pattern === "") {
    return true;
  }

  str = str.toLowerCase();
  pattern = pattern.toLowerCase();

  let strIdx = 0;
  let patIdx = 0;

  while (strIdx < str.length && patIdx < pattern.length) {
    if (str[strIdx] === pattern[patIdx]) {
      patIdx++;
    }
    strIdx++;
  }

  return patIdx >= pattern.length;
}

// Filter files by query using fuzzy matching
function filterFiles(files: string[], query: string): string[] {
  if (query === "" || query.trim() === "") {
    // Return first 100 files for empty query
    return files.slice(0, 100);
  }

  const results: string[] = [];
  for (const file of files) {
    if (fuzzyMatch(file, query)) {
      results.push(file);
      if (results.length >= 100) {
        break;
      }
    }
  }

  return results;
}

// Load git-tracked files asynchronously
async function loadGitFiles(): Promise<void> {
  if (isLoading) {
    return;
  }

  isLoading = true;
  editor.setStatus("Loading git files...");

  try {
    const result = await editor.spawnProcess("git", ["ls-files"]);

    if (result.exit_code === 0) {
      allFiles = result.stdout
        .split("\n")
        .filter(line => line.trim() !== "");

      editor.debug(`Loaded ${allFiles.length} git-tracked files`);
      editor.setStatus(`Git Find File: ${allFiles.length} files indexed`);
    } else {
      editor.debug(`Failed to load git files: ${result.stderr}`);
      editor.setStatus(`Error loading git files: ${result.stderr}`);
      allFiles = [];
    }
  } catch (e) {
    editor.debug(`Exception loading git files: ${e}`);
    editor.setStatus("Failed to load git files");
    allFiles = [];
  } finally {
    isLoading = false;
  }
}

// Open a git-tracked file by partial name match
globalThis.git_find_file_open = async function(): Promise<void> {
  if (allFiles.length === 0 && !isLoading) {
    await loadGitFiles();
  }

  if (allFiles.length === 0) {
    editor.setStatus("No git-tracked files found");
    return;
  }

  // For now, just show the first 10 files in status
  // Full prompt API not yet available
  const preview = allFiles.slice(0, 10).join(", ");
  editor.setStatus(`Files available: ${preview}...`);
  editor.debug("Full prompt UI not yet available in TypeScript runtime");
};

// Quick open: directly open a file if exact match exists
globalThis.git_quick_open = async function(pattern: string): Promise<void> {
  if (allFiles.length === 0 && !isLoading) {
    await loadGitFiles();
  }

  if (allFiles.length === 0) {
    editor.setStatus("No git-tracked files found");
    return;
  }

  const matches = filterFiles(allFiles, pattern);

  if (matches.length === 0) {
    editor.setStatus(`No files matching '${pattern}'`);
  } else if (matches.length === 1) {
    const file = matches[0];
    editor.openFile(file, 1, 1);
    editor.setStatus(`Opened ${file}`);
  } else {
    editor.setStatus(`Found ${matches.length} matches: ${matches.slice(0, 5).join(", ")}...`);
  }
};

// Show file count
globalThis.git_file_count = function(): void {
  if (allFiles.length === 0) {
    editor.setStatus("No git files loaded. Run 'Git Find File: Reload' first.");
  } else {
    editor.setStatus(`${allFiles.length} git-tracked files indexed`);
  }
};

// Reload git files
globalThis.git_reload_files = async function(): Promise<void> {
  allFiles = [];
  await loadGitFiles();
};

// Register commands
editor.registerCommand(
  "Git Find File: Open",
  "Find and open a git-tracked file (limited UI)",
  "git_find_file_open",
  "normal"
);

editor.registerCommand(
  "Git Find File: Count",
  "Show number of indexed git files",
  "git_file_count",
  "normal"
);

editor.registerCommand(
  "Git Find File: Reload",
  "Reload the git file index",
  "git_reload_files",
  "normal"
);

// Load git files on plugin initialization
loadGitFiles();

editor.debug("Git Find File plugin loaded (TypeScript version)");
editor.debug("Note: Full interactive prompt UI not yet available");
editor.debug("Use git_quick_open('pattern') for direct file opening");
